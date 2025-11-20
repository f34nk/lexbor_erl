#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Lexbor */
#include <lexbor/html/parser.h>
#include <lexbor/dom/interfaces/document.h>
#include <lexbor/dom/interfaces/element.h>
#include <lexbor/dom/interfaces/node.h>
#include <lexbor/html/serialize.h>
#include <lexbor/selectors/selectors.h>
#include <lexbor/css/css.h>

/* ---------- IO framing ({packet,4}) ---------- */
static int read_exact(unsigned char *b, int len){ 
    int i=0; 
    while(i<len){
        int g=fread(b+i,1,len-i,stdin); 
        if(g<=0)return 0; 
        i+=g;
    } 
    return 1; 
}

static int write_exact(const unsigned char *b, int len){ 
    int i=0; 
    while(i<len){
        int p=fwrite(b+i,1,len-i,stdout); 
        if(p<=0)return 0; 
        i+=p;
    } 
    fflush(stdout); 
    return 1; 
}

static int read_packet(unsigned char **data, uint32_t *len_out){
    unsigned char h[4]; 
    if(!read_exact(h,4)) return 0;
    uint32_t len=(h[0]<<24)|(h[1]<<16)|(h[2]<<8)|h[3];
    unsigned char *buf=(unsigned char*)malloc(len); 
    if(!buf) return 0;
    if(!read_exact(buf,len)){ free(buf); return 0; }
    *data=buf; *len_out=len; return 1;
}

static int write_packet(const unsigned char *data, uint32_t len){
    unsigned char h[4]={(len>>24)&0xFF,(len>>16)&0xFF,(len>>8)&0xFF,(len)&0xFF};
    return write_exact(h,4) && write_exact(data,len);
}

/* ---------- tiny buf for serializer ---------- */
typedef struct { unsigned char *ptr; size_t len, cap; } sbuf_t;

static int sbuf_reserve(sbuf_t *b, size_t need){
    if(b->len + need <= b->cap) return 1;
    size_t cap=b->cap? b->cap : 1024; 
    while(cap < b->len+need) cap*=2;
    unsigned char *np=(unsigned char*)realloc(b->ptr, cap); 
    if(!np) return 0;
    b->ptr=np; b->cap=cap; return 1;
}

static lxb_status_t sink_cb(const lxb_char_t *data, size_t len, void *ctx){
    sbuf_t *b=(sbuf_t*)ctx; 
    if(!sbuf_reserve(b,len)) return LXB_STATUS_ERROR_MEMORY_ALLOCATION;
    memcpy(b->ptr + b->len, data, len); 
    b->len += len; 
    return LXB_STATUS_OK;
}

/* ---------- tag utils ---------- */
static int tag_eq(const unsigned char tag[16], const char *name){
    size_t n=strlen(name); 
    if(n>16) n=16;
    if(memcmp(tag, name, n)!=0) return 0;
    for(size_t i=n;i<16;i++) if(tag[i]!=0) return 0;
    return 1;
}

/* ---------- document registry ---------- */
typedef struct { uint64_t id; lxb_html_document_t *doc; } Doc;
static Doc *docs=NULL; 
static size_t docs_len=0, docs_cap=0; 
static uint64_t next_id=1;

static Doc* add_doc(lxb_html_document_t *d){
    if(docs_len==docs_cap){ 
        size_t nc = docs_cap? docs_cap*2 : 16;
        Doc *nd=(Doc*)realloc(docs, nc*sizeof(Doc)); 
        if(!nd) return NULL;
        docs=nd; docs_cap=nc;
    }
    docs[docs_len].id = next_id++;
    docs[docs_len].doc = d;
    return &docs[docs_len++];
}

static Doc* find_doc(uint64_t id){
    for(size_t i=0;i<docs_len;i++) 
        if(docs[i].id==id) return &docs[i];
    return NULL;
}

static int remove_doc(uint64_t id){
    for(size_t i=0;i<docs_len;i++){
        if(docs[i].id==id){
            lxb_html_document_destroy(docs[i].doc);
            docs[i]=docs[docs_len-1];
            docs_len--;
            return 1;
        }
    }
    return 0;
}

/* ---------- selector matching callback context ---------- */
typedef struct {
    uint64_t *arr;
    size_t cap;
    size_t cnt;
} match_ctx_t;

static lxb_status_t selector_match_cb(lxb_dom_node_t *node, lxb_css_selector_specificity_t spec, void *ctx){
    (void)spec;
    match_ctx_t *mc = (match_ctx_t*)ctx;
    
    if(mc->cnt == mc->cap){
        size_t nc = mc->cap * 2;
        uint64_t *na = (uint64_t*)realloc(mc->arr, nc * sizeof(uint64_t));
        if(!na) return LXB_STATUS_ERROR_MEMORY_ALLOCATION;
        mc->arr = na;
        mc->cap = nc;
    }
    /* Use pointer value as opaque handle */
    mc->arr[mc->cnt++] = (uint64_t)(uintptr_t)node;
    return LXB_STATUS_OK;
}

/* Callback for SELECT_HTML operation - collects HTML fragments */
typedef struct {
    sbuf_t *items;
    size_t cap;
    size_t count;
} html_ctx_t;

static lxb_status_t html_match_cb(lxb_dom_node_t *node, lxb_css_selector_specificity_t spec, void *ctx){
    (void)spec;
    html_ctx_t *hc = (html_ctx_t*)ctx;
    
    if(hc->count == hc->cap){
        size_t ncap = hc->cap * 2;
        sbuf_t *nitems = (sbuf_t*)realloc(hc->items, ncap * sizeof(sbuf_t));
        if(!nitems) return LXB_STATUS_ERROR_MEMORY_ALLOCATION;
        /* zero new area */
        memset(nitems + hc->cap, 0, (ncap - hc->cap) * sizeof(sbuf_t));
        hc->items = nitems;
        hc->cap = ncap;
    }
    
    sbuf_t *slot = &hc->items[hc->count];
    lxb_status_t s = lxb_html_serialize_tree_cb(node, sink_cb, slot);
    if(s != LXB_STATUS_OK) return s;
    
    hc->count++;
    return LXB_STATUS_OK;
}

/* ---------- operations ---------- */
static int op_parse_doc(const unsigned char *payload, uint32_t plen,
                        unsigned char **out, uint32_t *outlen){
    lxb_html_document_t *doc = lxb_html_document_create();
    if(!doc) goto mem_err;
    
    lxb_status_t st = lxb_html_document_parse(doc, (const lxb_char_t*)payload, (size_t)plen);
    if(st != LXB_STATUS_OK){ 
        lxb_html_document_destroy(doc);
        const char *msg="parse_error";
        uint32_t bl=1 + (uint32_t)strlen(msg);
        unsigned char *b=(unsigned char*)malloc(bl); 
        if(!b) goto mem_err;
        b[0]=1; memcpy(b+1,msg,strlen(msg));
        *out=b; *outlen=bl; return 1;
    }
    
    Doc *d = add_doc(doc);
    if(!d){ lxb_html_document_destroy(doc); goto mem_err; }
    
    unsigned char *b=(unsigned char*)malloc(1+8); 
    if(!b) goto mem_err;
    b[0]=0;
    /* DocId big-endian */
    uint64_t id=d->id;
    b[1]=(id>>56)&0xFF; b[2]=(id>>48)&0xFF; b[3]=(id>>40)&0xFF; b[4]=(id>>32)&0xFF;
    b[5]=(id>>24)&0xFF; b[6]=(id>>16)&0xFF; b[7]=(id>>8)&0xFF;  b[8]=id&0xFF;
    *out=b; *outlen=9; return 1;
    
mem_err:
    { 
        const char *e="oom"; 
        uint32_t bl=1+(uint32_t)strlen(e);
        unsigned char *b=(unsigned char*)malloc(bl); 
        if(!b) return 0;
        b[0]=1; memcpy(b+1,e,strlen(e)); 
        *out=b; *outlen=bl; return 1; 
    }
}

static int op_release_doc(const unsigned char *payload, uint32_t plen,
                          unsigned char **out, uint32_t *outlen){
    if(plen<8){ return 0; }
    uint64_t id = ((uint64_t)payload[0]<<56)|((uint64_t)payload[1]<<48)|
                  ((uint64_t)payload[2]<<40)|((uint64_t)payload[3]<<32)|
                  ((uint64_t)payload[4]<<24)|((uint64_t)payload[5]<<16)|
                  ((uint64_t)payload[6]<<8)|((uint64_t)payload[7]);
    int ok = remove_doc(id);
    unsigned char *b=(unsigned char*)malloc(1 + (ok?0:4));
    if(!b) return 0;
    if(ok){ b[0]=0; *out=b; *outlen=1; return 1; }
    b[0]=1; b[1]=b[2]=b[3]=b[4]=0; 
    *out=b; *outlen=5; return 1;
}

static int op_select_nodes(const unsigned char *payload, uint32_t plen,
                           unsigned char **out, uint32_t *outlen){
    if(plen<8+4) return 0;
    
    /* DocId (8) + SelLen (4) + Sel */
    uint64_t id = ((uint64_t)payload[0]<<56)|((uint64_t)payload[1]<<48)|
                  ((uint64_t)payload[2]<<40)|((uint64_t)payload[3]<<32)|
                  ((uint64_t)payload[4]<<24)|((uint64_t)payload[5]<<16)|
                  ((uint64_t)payload[6]<<8)|((uint64_t)payload[7]);
    uint32_t sl = (payload[8]<<24)|(payload[9]<<16)|(payload[10]<<8)|payload[11];
    if(12+sl > plen) return 0;
    const unsigned char *sel = payload+12;

    Doc *d = find_doc(id);
    if(!d){
        unsigned char *b=(unsigned char*)malloc(1+1); 
        if(!b) return 0;
        b[0]=1; b[1]='?'; 
        *out=b; *outlen=2; return 1;
    }

    /* Create CSS parser */
    lxb_css_parser_t *parser = lxb_css_parser_create();
    if(!parser){
        unsigned char *b=(unsigned char*)malloc(2); 
        if(!b) return 0;
        b[0]=1; b[1]='m'; 
        *out=b; *outlen=2; return 1;
    }
    lxb_status_t st = lxb_css_parser_init(parser, NULL);
    if(st != LXB_STATUS_OK){
        lxb_css_parser_destroy(parser, true);
        unsigned char *b=(unsigned char*)malloc(2); 
        if(!b) return 0;
        b[0]=1; b[1]='i'; 
        *out=b; *outlen=2; return 1;
    }

    /* Parse selector */
    lxb_css_selector_list_t *list = lxb_css_selectors_parse(parser, sel, sl);
    if(!list){
        lxb_css_parser_destroy(parser, true);
        const char *msg="bad_selector";
        unsigned char *b=(unsigned char*)malloc(1+strlen(msg)); 
        if(!b) return 0;
        b[0]=1; memcpy(b+1,msg,strlen(msg)); 
        *out=b; *outlen=1+strlen(msg); return 1;
    }

    /* Create selectors engine */
    lxb_selectors_t *selectors = lxb_selectors_create();
    if(!selectors){
        lxb_css_selector_list_destroy_memory(list);
        lxb_css_parser_destroy(parser, true);
        unsigned char *b=(unsigned char*)malloc(2); 
        if(!b) return 0;
        b[0]=1; b[1]='s'; 
        *out=b; *outlen=2; return 1;
    }
    st = lxb_selectors_init(selectors);
    if(st != LXB_STATUS_OK){
        lxb_selectors_destroy(selectors, true);
        lxb_css_selector_list_destroy_memory(list);
        lxb_css_parser_destroy(parser, true);
        unsigned char *b=(unsigned char*)malloc(2); 
        if(!b) return 0;
        b[0]=1; b[1]='I'; 
        *out=b; *outlen=2; return 1;
    }

    /* Collect handles */
    match_ctx_t mc = {0};
    mc.cap = 8;
    mc.arr = (uint64_t*)malloc(mc.cap * sizeof(uint64_t));
    if(!mc.arr){
        lxb_selectors_destroy(selectors, true);
        lxb_css_selector_list_destroy_memory(list);
        lxb_css_parser_destroy(parser, true);
        return 0;
    }

    lxb_dom_node_t *root = lxb_dom_interface_node(
        lxb_dom_document_element(lxb_dom_interface_document(d->doc))
    );

    st = lxb_selectors_find(selectors, root, list, selector_match_cb, &mc);
    
    lxb_selectors_destroy(selectors, true);
    lxb_css_selector_list_destroy_memory(list);
    lxb_css_parser_destroy(parser, true);
    
    if(st != LXB_STATUS_OK){ 
        free(mc.arr); 
        return 0; 
    }

    uint32_t bl = 1 + 4 + (uint32_t)(mc.cnt * 8);
    unsigned char *b=(unsigned char*)malloc(bl); 
    if(!b){ free(mc.arr); return 0; }
    
    b[0]=0; 
    b[1]=(mc.cnt>>24)&0xFF; b[2]=(mc.cnt>>16)&0xFF; 
    b[3]=(mc.cnt>>8)&0xFF; b[4]=mc.cnt&0xFF;
    
    unsigned char *p=b+5;
    for(size_t i=0; i<mc.cnt; i++){
        uint64_t h=mc.arr[i];
        p[0]=(h>>56)&0xFF; p[1]=(h>>48)&0xFF; p[2]=(h>>40)&0xFF; p[3]=(h>>32)&0xFF;
        p[4]=(h>>24)&0xFF; p[5]=(h>>16)&0xFF; p[6]=(h>>8)&0xFF; p[7]=h&0xFF; 
        p+=8;
    }
    free(mc.arr);
    *out=b; *outlen=bl; return 1;
}

static int op_outer_html(const unsigned char *payload, uint32_t plen,
                         unsigned char **out, uint32_t *outlen){
    if(plen<16) return 0;
    
    uint64_t id = ((uint64_t)payload[0]<<56)|((uint64_t)payload[1]<<48)|
                  ((uint64_t)payload[2]<<40)|((uint64_t)payload[3]<<32)|
                  ((uint64_t)payload[4]<<24)|((uint64_t)payload[5]<<16)|
                  ((uint64_t)payload[6]<<8)|((uint64_t)payload[7]);
    uint64_t h  = ((uint64_t)payload[8]<<56)|((uint64_t)payload[9]<<48)|
                  ((uint64_t)payload[10]<<40)|((uint64_t)payload[11]<<32)|
                  ((uint64_t)payload[12]<<24)|((uint64_t)payload[13]<<16)|
                  ((uint64_t)payload[14]<<8)|((uint64_t)payload[15]);

    Doc *d=find_doc(id);
    if(!d){
        unsigned char *b=(unsigned char*)malloc(2); 
        if(!b) return 0;
        b[0]=1; b[1]='?'; 
        *out=b; *outlen=2; return 1;
    }
    
    lxb_dom_node_t *node=(lxb_dom_node_t*)(uintptr_t)h;

    sbuf_t buf={0};
    lxb_status_t st = lxb_html_serialize_tree_cb(node, sink_cb, &buf);
    if(st!=LXB_STATUS_OK){ 
        if(buf.ptr) free(buf.ptr); 
        return 0; 
    }

    uint32_t bl = 1 + 4 + (uint32_t)buf.len;
    unsigned char *b=(unsigned char*)malloc(bl); 
    if(!b){ free(buf.ptr); return 0; }
    
    b[0]=0; 
    b[1]=(buf.len>>24)&0xFF; b[2]=(buf.len>>16)&0xFF; 
    b[3]=(buf.len>>8)&0xFF; b[4]=buf.len&0xFF;
    memcpy(b+5, buf.ptr, buf.len);
    free(buf.ptr);
    
    *out=b; *outlen=bl; return 1;
}

/* Serialize full document for PARSE_SERIALIZE */
static int serialize_full_doc(lxb_html_document_t *doc, sbuf_t *out) {
    lxb_dom_node_t *html_node = lxb_dom_interface_node(
        lxb_dom_document_element(lxb_dom_interface_document(doc))
    );
    lxb_status_t status = lxb_html_serialize_tree_cb(html_node, sink_cb, out);
    return status == LXB_STATUS_OK;
}

/* Run CSS selector and collect matches' outerHTML for SELECT_HTML */
static int select_outer_html(lxb_html_document_t *doc,
                             const unsigned char *sel, uint32_t sel_len,
                             unsigned char **out_buf, uint32_t *out_len){
    /* Create CSS parser */
    lxb_css_parser_t *parser = lxb_css_parser_create();
    if(!parser) return 0;
    
    lxb_status_t status = lxb_css_parser_init(parser, NULL);
    if(status != LXB_STATUS_OK){ 
        lxb_css_parser_destroy(parser, true); 
        return 0; 
    }

    /* Parse selector */
    lxb_css_selector_list_t *list = lxb_css_selectors_parse(parser, sel, sel_len);
    if(!list){ 
        lxb_css_parser_destroy(parser, true); 
        return 0; 
    }

    /* Create selectors engine */
    lxb_selectors_t *selectors = lxb_selectors_create();
    if(!selectors){ 
        lxb_css_selector_list_destroy_memory(list);
        lxb_css_parser_destroy(parser, true); 
        return 0; 
    }
    
    status = lxb_selectors_init(selectors);
    if(status != LXB_STATUS_OK){ 
        lxb_selectors_destroy(selectors, true);
        lxb_css_selector_list_destroy_memory(list);
        lxb_css_parser_destroy(parser, true); 
        return 0; 
    }

    /* Root node */
    lxb_dom_node_t *root = lxb_dom_interface_node(
        lxb_dom_document_element(lxb_dom_interface_document(doc))
    );

    /* Collect HTML fragments */
    html_ctx_t hc = {0};
    hc.cap = 8;
    hc.items = (sbuf_t*)calloc(hc.cap, sizeof(sbuf_t));
    if(!hc.items){ 
        lxb_selectors_destroy(selectors, true);
        lxb_css_selector_list_destroy_memory(list);
        lxb_css_parser_destroy(parser, true); 
        return 0; 
    }

    /* Find matches */
    status = lxb_selectors_find(selectors, root, list, html_match_cb, &hc);
    
    lxb_selectors_destroy(selectors, true);
    lxb_css_selector_list_destroy_memory(list);
    lxb_css_parser_destroy(parser, true);

    if(status != LXB_STATUS_OK){
        for(size_t i = 0; i < hc.count; i++) free(hc.items[i].ptr);
        free(hc.items);
        return 0;
    }

    /* Pack into <<Count:32, [Len:32, Bin]*>> */
    uint32_t total = 4;
    for(size_t i = 0; i < hc.count; i++) 
        total += 4 + (uint32_t)hc.items[i].len;

    unsigned char *buf = (unsigned char*)malloc(total);
    if(!buf){
        for(size_t i = 0; i < hc.count; i++) free(hc.items[i].ptr);
        free(hc.items);
        return 0;
    }

    unsigned char *p = buf;
    p[0] = (hc.count >> 24) & 0xFF; p[1] = (hc.count >> 16) & 0xFF; 
    p[2] = (hc.count >> 8) & 0xFF; p[3] = (hc.count) & 0xFF;
    p += 4;

    for(size_t i = 0; i < hc.count; i++){
        uint32_t L = (uint32_t)hc.items[i].len;
        p[0] = (L >> 24) & 0xFF; p[1] = (L >> 16) & 0xFF; 
        p[2] = (L >> 8) & 0xFF; p[3] = (L) & 0xFF;
        p += 4;
        memcpy(p, hc.items[i].ptr, L);
        p += L;
        free(hc.items[i].ptr);
    }
    free(hc.items);

    *out_buf = buf; 
    *out_len = total;
    return 1;
}

/* ---------- main loop ---------- */
int main(void){
    for(;;){
        unsigned char *req=NULL; 
        uint32_t rlen=0;
        if(!read_packet(&req,&rlen)) break;
        if(rlen<17){ free(req); break; }

        unsigned char ver=req[0];
        unsigned char tag[16]; 
        memcpy(tag, req+1, 16);
        unsigned char *payload=req+17; 
        uint32_t plen=rlen-17;

        unsigned char *resp=NULL; 
        uint32_t rplen=0;

        if(tag_eq(tag,"PARSE_DOC")){
            if(!op_parse_doc(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"RELEASE_DOC")){
            if(!op_release_doc(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"SELECT_NODES")){
            if(!op_select_nodes(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"OUTER_HTML")){
            if(!op_outer_html(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"SELECT_HTML")){
            if(plen < 4) { free(req); break; }
            uint32_t sel_len = (payload[0]<<24) | (payload[1]<<16) | 
                              (payload[2]<<8) | payload[3];
            if(4 + sel_len > plen) { free(req); break; }
            unsigned char *sel = payload + 4;
            unsigned char *html = payload + 4 + sel_len;
            uint32_t html_len = plen - 4 - sel_len;

            lxb_html_document_t *doc = lxb_html_document_create();
            if(doc == NULL) { free(req); break; }

            lxb_status_t status = lxb_html_document_parse(doc, 
                                        (const lxb_char_t*)html, (size_t)html_len);
            if(status != LXB_STATUS_OK){
                lxb_html_document_destroy(doc);
                /* Return empty list: <<0:32>> */
                uint32_t blen = 1 + 16 + 4;
                unsigned char *buf = (unsigned char*)malloc(blen);
                if(!buf) { free(req); break; }
                buf[0] = ver; memcpy(buf + 1, tag, 16);
                buf[17] = buf[18] = buf[19] = buf[20] = 0;
                int ok = write_packet(buf, blen);
                free(buf); free(req);
                if(!ok) break;
                continue;
            }

            unsigned char *packed = NULL; 
            uint32_t packed_len = 0;
            int ok_pack = select_outer_html(doc, sel, sel_len, &packed, &packed_len);

            lxb_html_document_destroy(doc);

            if(!ok_pack) { free(req); break; }

            uint32_t blen = 1 + 16 + packed_len;
            unsigned char *buf = (unsigned char*)malloc(blen);
            if(!buf) { free(packed); free(req); break; }
            buf[0] = ver; memcpy(buf + 1, tag, 16); memcpy(buf + 17, packed, packed_len);

            int ok = write_packet(buf, blen);
            free(packed); free(buf); free(req);
            if(!ok) break;
            continue;
        }
        else if(tag_eq(tag,"PARSE_SERIALIZE")){
            lxb_html_document_t *doc = lxb_html_document_create();
            if(!doc){ free(req); break; }
            
            lxb_status_t st=lxb_html_document_parse(doc,
                                (const lxb_char_t*)payload,(size_t)plen);
            sbuf_t out={0}; 
            int ok=(st==LXB_STATUS_OK) && serialize_full_doc(doc, &out);
            
            if(!ok){
                uint32_t bl=1+16+plen; 
                unsigned char *b=(unsigned char*)malloc(bl);
                if(!b){ lxb_html_document_destroy(doc); free(req); break; }
                b[0]=ver; memcpy(b+1,tag,16); memcpy(b+17,payload,plen);
                int w=write_packet(b, bl); 
                free(b); lxb_html_document_destroy(doc); free(req); 
                if(!w) break; 
                continue;
            }
            
            uint32_t bl=1+16+(uint32_t)out.len; 
            unsigned char *b=(unsigned char*)malloc(bl);
            if(!b){ free(out.ptr); lxb_html_document_destroy(doc); free(req); break; }
            b[0]=ver; memcpy(b+1,tag,16); memcpy(b+17,out.ptr,out.len);
            int w=write_packet(b, bl); 
            free(out.ptr); lxb_html_document_destroy(doc); free(b); free(req); 
            if(!w) break; 
            continue;
        }
        else {
            /* unknown tag -> error status */
            const char *msg="unknown_tag";
            rplen=1+strlen(msg); 
            resp=(unsigned char*)malloc(rplen);
            if(!resp){ free(req); break; }
            resp[0]=1; memcpy(resp+1,msg,strlen(msg));
        }

        /* wrap reply with [ver][tag16][payload] */
        uint32_t bl=1+16+rplen;
        unsigned char *pkt=(unsigned char*)malloc(bl); 
        if(!pkt){ free(resp); free(req); break; }
        pkt[0]=ver; memcpy(pkt+1,tag,16); memcpy(pkt+17,resp,rplen);
        int w = write_packet(pkt, bl);
        free(pkt); free(resp); free(req);
        if(!w) break;
    }
    return 0;
}
