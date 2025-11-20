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

/* ---------- parse session registry ---------- */
typedef struct {
    uint64_t session_id;
    lxb_html_document_t *doc;
    int begun;
    int ended;
    size_t chunks_processed;
} ParseSession;

static ParseSession *sessions = NULL;
static size_t sessions_len = 0, sessions_cap = 0;
static uint64_t next_session_id = 1;

static ParseSession* add_session(lxb_html_document_t *d) {
    if (sessions_len == sessions_cap) {
        size_t nc = sessions_cap ? sessions_cap * 2 : 16;
        ParseSession *ns = (ParseSession*)realloc(sessions, nc * sizeof(ParseSession));
        if (!ns) return NULL;
        sessions = ns;
        sessions_cap = nc;
    }
    sessions[sessions_len].session_id = next_session_id++;
    sessions[sessions_len].doc = d;
    sessions[sessions_len].begun = 0;
    sessions[sessions_len].ended = 0;
    sessions[sessions_len].chunks_processed = 0;
    return &sessions[sessions_len++];
}

static ParseSession* find_session(uint64_t id) {
    for (size_t i = 0; i < sessions_len; i++)
        if (sessions[i].session_id == id) return &sessions[i];
    return NULL;
}

static int remove_session(uint64_t id) {
    for (size_t i = 0; i < sessions_len; i++) {
        if (sessions[i].session_id == id) {
            // Note: Document is NOT destroyed here as it's moved to doc registry
            sessions[i] = sessions[sessions_len - 1];
            sessions_len--;
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

/* ---------- Helper functions for DOM manipulation ---------- */

/* Read uint64 from big-endian bytes */
static uint64_t read_uint64(const unsigned char *buf) {
    return ((uint64_t)buf[0] << 56) | ((uint64_t)buf[1] << 48) |
           ((uint64_t)buf[2] << 40) | ((uint64_t)buf[3] << 32) |
           ((uint64_t)buf[4] << 24) | ((uint64_t)buf[5] << 16) |
           ((uint64_t)buf[6] << 8)  | ((uint64_t)buf[7]);
}

/* Write uint64 to big-endian bytes */
static void write_uint64(unsigned char *buf, uint64_t val) {
    buf[0] = (val >> 56) & 0xFF;
    buf[1] = (val >> 48) & 0xFF;
    buf[2] = (val >> 40) & 0xFF;
    buf[3] = (val >> 32) & 0xFF;
    buf[4] = (val >> 24) & 0xFF;
    buf[5] = (val >> 16) & 0xFF;
    buf[6] = (val >> 8) & 0xFF;
    buf[7] = val & 0xFF;
}

/* Read uint32 from big-endian bytes */
static uint32_t read_uint32(const unsigned char *buf) {
    return ((uint32_t)buf[0] << 24) | ((uint32_t)buf[1] << 16) |
           ((uint32_t)buf[2] << 8)  | ((uint32_t)buf[3]);
}

/* Write uint32 to big-endian bytes */
static void write_uint32(unsigned char *buf, uint32_t val) {
    buf[0] = (val >> 24) & 0xFF;
    buf[1] = (val >> 16) & 0xFF;
    buf[2] = (val >> 8) & 0xFF;
    buf[3] = val & 0xFF;
}

/* Create error response */
static int error_response(const char *msg, unsigned char **out, uint32_t *outlen) {
    size_t len = strlen(msg);
    unsigned char *buf = (unsigned char*)malloc(1 + len);
    if (!buf) return 0;
    buf[0] = 1;  /* Error marker */
    memcpy(buf + 1, msg, len);
    *out = buf;
    *outlen = 1 + len;
    return 1;
}

/* ---------- DOM Manipulation Operations ---------- */

/* GET_ATTRIBUTE: Get attribute value from element */
static int op_get_attribute(const unsigned char *payload, uint32_t plen,
                            unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8 + 4) return 0;
    
    /* Payload: <<DocId:64, NodeHandle:64, AttrNameLen:32, AttrName/binary>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t node_handle = read_uint64(payload + 8);
    uint32_t attr_len = read_uint32(payload + 16);
    
    if (20 + attr_len > plen) return 0;
    const unsigned char *attr_name = payload + 20;
    
    /* Find document */
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    /* Get node from handle */
    lxb_dom_node_t *node = (lxb_dom_node_t *)(uintptr_t)node_handle;
    
    /* Verify node belongs to document */
    if (node->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    /* Only works for element nodes */
    if (node->type != LXB_DOM_NODE_TYPE_ELEMENT) {
        return error_response("not_element", out, outlen);
    }
    
    lxb_dom_element_t *element = lxb_dom_interface_element(node);
    lxb_dom_attr_t *attr = lxb_dom_element_attr_by_name(
        element, attr_name, attr_len
    );
    
    if (!attr || !attr->value) {
        /* Attribute not found - return special marker */
        unsigned char *buf = (unsigned char*)malloc(1);
        if (!buf) return 0;
        buf[0] = 2;  /* Special code for "not found" */
        *out = buf;
        *outlen = 1;
        return 1;
    }
    
    /* Return attribute value */
    size_t value_len;
    const lxb_char_t *value = lxb_dom_attr_value(attr, &value_len);
    
    /* Response: <<0:8, ValueLen:32, Value/binary>> */
    uint32_t resp_len = 1 + 4 + value_len;
    unsigned char *buf = (unsigned char*)malloc(resp_len);
    if (!buf) return 0;
    
    buf[0] = 0;  /* Success */
    write_uint32(buf + 1, value_len);
    memcpy(buf + 5, value, value_len);
    
    *out = buf;
    *outlen = resp_len;
    return 1;
}

/* SET_ATTRIBUTE: Set attribute value on element */
static int op_set_attribute(const unsigned char *payload, uint32_t plen,
                            unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8 + 4) return 0;
    
    /* Payload: <<DocId:64, NodeHandle:64, AttrNameLen:32, AttrName/binary,
                 ValueLen:32, Value/binary>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t node_handle = read_uint64(payload + 8);
    uint32_t attr_len = read_uint32(payload + 16);
    
    if (20 + attr_len + 4 > plen) return 0;
    const unsigned char *attr_name = payload + 20;
    
    uint32_t value_len = read_uint32(payload + 20 + attr_len);
    if (24 + attr_len + value_len > plen) return 0;
    const unsigned char *value = payload + 24 + attr_len;
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    lxb_dom_node_t *node = (lxb_dom_node_t *)(uintptr_t)node_handle;
    
    if (node->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    if (node->type != LXB_DOM_NODE_TYPE_ELEMENT) {
        return error_response("not_element", out, outlen);
    }
    
    lxb_dom_element_t *element = lxb_dom_interface_element(node);
    
    /* Set attribute */
    lxb_dom_attr_t *attr = lxb_dom_element_set_attribute(
        element, attr_name, attr_len, value, value_len
    );
    
    if (!attr) {
        return error_response("set_failed", out, outlen);
    }
    
    /* Success response */
    unsigned char *buf = (unsigned char*)malloc(1);
    if (!buf) return 0;
    buf[0] = 0;  /* Success */
    *out = buf;
    *outlen = 1;
    return 1;
}

/* REMOVE_ATTRIBUTE: Remove attribute from element */
static int op_remove_attribute(const unsigned char *payload, uint32_t plen,
                               unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8 + 4) return 0;
    
    /* Payload: <<DocId:64, NodeHandle:64, AttrNameLen:32, AttrName/binary>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t node_handle = read_uint64(payload + 8);
    uint32_t attr_len = read_uint32(payload + 16);
    
    if (20 + attr_len > plen) return 0;
    const unsigned char *attr_name = payload + 20;
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    lxb_dom_node_t *node = (lxb_dom_node_t *)(uintptr_t)node_handle;
    
    if (node->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    if (node->type != LXB_DOM_NODE_TYPE_ELEMENT) {
        return error_response("not_element", out, outlen);
    }
    
    lxb_dom_element_t *element = lxb_dom_interface_element(node);
    
    /* Remove attribute */
    lxb_status_t status = lxb_dom_element_remove_attribute(
        element, attr_name, attr_len
    );
    
    if (status != LXB_STATUS_OK) {
        return error_response("remove_failed", out, outlen);
    }
    
    /* Success response */
    unsigned char *buf = (unsigned char*)malloc(1);
    if (!buf) return 0;
    buf[0] = 0;  /* Success */
    *out = buf;
    *outlen = 1;
    return 1;
}

/* GET_TEXT: Get text content of a node (recursively) */
static int op_get_text(const unsigned char *payload, uint32_t plen,
                       unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8) return 0;
    
    /* Payload: <<DocId:64, NodeHandle:64>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t node_handle = read_uint64(payload + 8);
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    lxb_dom_node_t *node = (lxb_dom_node_t *)(uintptr_t)node_handle;
    
    if (node->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    /* Collect text content */
    size_t text_len;
    const lxb_char_t *text = lxb_dom_node_text_content(node, &text_len);
    
    if (!text && text_len > 0) {
        return error_response("text_extraction_failed", out, outlen);
    }
    
    /* Response: <<0:8, TextLen:32, Text/binary>> */
    uint32_t resp_len = 1 + 4 + text_len;
    unsigned char *resp = (unsigned char*)malloc(resp_len);
    if (!resp) {
        return 0;
    }
    
    resp[0] = 0;  /* Success */
    write_uint32(resp + 1, text_len);
    if (text_len > 0 && text) {
        memcpy(resp + 5, text, text_len);
    }
    
    *out = resp;
    *outlen = resp_len;
    return 1;
}

/* SET_TEXT: Set text content of a node (replaces all children) */
static int op_set_text(const unsigned char *payload, uint32_t plen,
                       unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8 + 4) return 0;
    
    /* Payload: <<DocId:64, NodeHandle:64, TextLen:32, Text/binary>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t node_handle = read_uint64(payload + 8);
    uint32_t text_len = read_uint32(payload + 16);
    
    if (20 + text_len > plen) return 0;
    const unsigned char *text = payload + 20;
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    lxb_dom_node_t *node = (lxb_dom_node_t *)(uintptr_t)node_handle;
    
    if (node->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    /* Remove all child nodes first */
    lxb_dom_node_t *child = node->first_child;
    while (child) {
        lxb_dom_node_t *next = child->next;
        lxb_dom_node_remove(child);
        lxb_dom_node_destroy(child);
        child = next;
    }
    
    /* Create and append text node */
    if (text_len > 0) {
        lxb_dom_document_t *doc = lxb_dom_interface_document(d->doc);
        lxb_dom_text_t *text_node = lxb_dom_document_create_text_node(
            doc, text, text_len
        );
        
        if (!text_node) {
            return error_response("create_text_failed", out, outlen);
        }
        
        lxb_dom_node_insert_child(node, lxb_dom_interface_node(text_node));
    }
    
    /* Success */
    unsigned char *buf = (unsigned char*)malloc(1);
    if (!buf) return 0;
    buf[0] = 0;
    *out = buf;
    *outlen = 1;
    return 1;
}

/* INNER_HTML: Get inner HTML of a node */
static int op_inner_html(const unsigned char *payload, uint32_t plen,
                         unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8) return 0;
    
    /* Payload: <<DocId:64, NodeHandle:64>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t node_handle = read_uint64(payload + 8);
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    lxb_dom_node_t *node = (lxb_dom_node_t *)(uintptr_t)node_handle;
    
    if (node->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    /* Serialize children */
    sbuf_t buf = {0};
    lxb_dom_node_t *child = node->first_child;
    while (child) {
        lxb_status_t status = lxb_html_serialize_tree_cb(child, sink_cb, &buf);
        if (status != LXB_STATUS_OK) {
            if (buf.ptr) free(buf.ptr);
            return error_response("serialize_failed", out, outlen);
        }
        child = child->next;
    }
    
    /* Response: <<0:8, HtmlLen:32, Html/binary>> */
    uint32_t resp_len = 1 + 4 + buf.len;
    unsigned char *resp = (unsigned char*)malloc(resp_len);
    if (!resp) {
        if (buf.ptr) free(buf.ptr);
        return 0;
    }
    
    resp[0] = 0;
    write_uint32(resp + 1, buf.len);
    if (buf.len > 0) {
        memcpy(resp + 5, buf.ptr, buf.len);
    }
    if (buf.ptr) free(buf.ptr);
    
    *out = resp;
    *outlen = resp_len;
    return 1;
}

/* SET_INNER_HTML: Set inner HTML of a node (parses and replaces children) */
static int op_set_inner_html(const unsigned char *payload, uint32_t plen,
                              unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8 + 4) return 0;
    
    /* Payload: <<DocId:64, NodeHandle:64, HtmlLen:32, Html/binary>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t node_handle = read_uint64(payload + 8);
    uint32_t html_len = read_uint32(payload + 16);
    
    if (20 + html_len > plen) return 0;
    const unsigned char *html = payload + 20;
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    lxb_dom_node_t *node = (lxb_dom_node_t *)(uintptr_t)node_handle;
    
    if (node->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    if (node->type != LXB_DOM_NODE_TYPE_ELEMENT) {
        return error_response("not_element", out, outlen);
    }
    
    lxb_dom_element_t *element = lxb_dom_interface_element(node);
    
    /* Remove all existing children */
    lxb_dom_node_t *child = node->first_child;
    while (child) {
        lxb_dom_node_t *next = child->next;
        lxb_dom_node_remove(child);
        lxb_dom_node_destroy(child);
        child = next;
    }
    
    /* Parse new HTML into a temporary document fragment */
    if (html_len > 0) {
        lxb_html_document_t *temp_doc = lxb_html_document_create();
        if (!temp_doc) {
            return error_response("create_temp_doc_failed", out, outlen);
        }
        
        lxb_status_t status = lxb_html_document_parse(temp_doc, html, html_len);
        if (status != LXB_STATUS_OK) {
            lxb_html_document_destroy(temp_doc);
            return error_response("parse_html_failed", out, outlen);
        }
        
        /* Move parsed nodes from temp document body to target element */
        lxb_dom_node_t *body = lxb_dom_interface_node(temp_doc->body);
        if (body) {
            lxb_dom_node_t *temp_child = body->first_child;
            while (temp_child) {
                lxb_dom_node_t *next = temp_child->next;
                lxb_dom_node_remove(temp_child);
                
                /* Append to target */
                lxb_dom_node_insert_child(node, temp_child);
                
                /* Change owner document recursively for the entire subtree 
                 * AFTER insertion, in case lxb_dom_node_insert_child modifies it */
                lxb_dom_node_t *walk = temp_child;
                while (walk) {
                    walk->owner_document = node->owner_document;
                    if (walk->first_child) {
                        walk = walk->first_child;
                    } else if (walk->next) {
                        walk = walk->next;
                    } else {
                        while (walk && !walk->next) {
                            walk = walk->parent;
                            if (walk == temp_child) {
                                walk = NULL;
                                break;
                            }
                        }
                        if (walk) walk = walk->next;
                    }
                }
                
                temp_child = next;
            }
        }
        
        lxb_html_document_destroy(temp_doc);
    }
    
    /* Success */
    unsigned char *buf = (unsigned char*)malloc(1);
    if (!buf) return 0;
    buf[0] = 0;
    *out = buf;
    *outlen = 1;
    return 1;
}

/* SERIALIZE_DOC: Serialize entire document to HTML */
static int op_serialize_doc(const unsigned char *payload, uint32_t plen,
                            unsigned char **out, uint32_t *outlen) {
    if (plen != 8) return 0;
    
    /* Payload: <<DocId:64>> */
    uint64_t doc_id = read_uint64(payload);
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    /* Serialize document */
    sbuf_t buf = {0};
    if (!serialize_full_doc(d->doc, &buf)) {
        return error_response("serialize_failed", out, outlen);
    }
    
    /* Response: <<0:8, HtmlLen:32, Html/binary>> */
    uint32_t resp_len = 1 + 4 + buf.len;
    unsigned char *resp = (unsigned char*)malloc(resp_len);
    if (!resp) {
        free(buf.ptr);
        return 0;
    }
    
    resp[0] = 0;
    write_uint32(resp + 1, buf.len);
    memcpy(resp + 5, buf.ptr, buf.len);
    free(buf.ptr);
    
    *out = resp;
    *outlen = resp_len;
    return 1;
}

/* CREATE_ELEMENT: Create a new element node */
static int op_create_element(const unsigned char *payload, uint32_t plen,
                             unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 4) return 0;
    
    /* Payload: <<DocId:64, TagNameLen:32, TagName/binary>> */
    uint64_t doc_id = read_uint64(payload);
    uint32_t tag_len = read_uint32(payload + 8);
    
    if (12 + tag_len > plen) return 0;
    const unsigned char *tag_name = payload + 12;
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    /* Create element */
    lxb_dom_document_t *doc = lxb_dom_interface_document(d->doc);
    lxb_dom_element_t *element = lxb_dom_document_create_element(
        doc, tag_name, tag_len, NULL
    );
    
    if (!element) {
        return error_response("create_failed", out, outlen);
    }
    
    /* Return node handle */
    /* Response: <<0:8, NodeHandle:64>> */
    unsigned char *buf = (unsigned char*)malloc(9);
    if (!buf) return 0;
    
    buf[0] = 0;  /* Success */
    uint64_t handle = (uint64_t)(uintptr_t)lxb_dom_interface_node(element);
    write_uint64(buf + 1, handle);
    
    *out = buf;
    *outlen = 9;
    return 1;
}

/* APPEND_CHILD: Append a child node to a parent */
static int op_append_child(const unsigned char *payload, uint32_t plen,
                           unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8 + 8) return 0;
    
    /* Payload: <<DocId:64, ParentHandle:64, ChildHandle:64>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t parent_handle = read_uint64(payload + 8);
    uint64_t child_handle = read_uint64(payload + 16);
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    lxb_dom_node_t *parent = (lxb_dom_node_t *)(uintptr_t)parent_handle;
    lxb_dom_node_t *child = (lxb_dom_node_t *)(uintptr_t)child_handle;
    
    /* Verify both nodes belong to document */
    if (parent->owner_document != lxb_dom_interface_document(d->doc) ||
        child->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    /* Append child */
    lxb_dom_node_insert_child(parent, child);
    
    /* Success */
    unsigned char *buf = (unsigned char*)malloc(1);
    if (!buf) return 0;
    buf[0] = 0;
    *out = buf;
    *outlen = 1;
    return 1;
}

/* INSERT_BEFORE: Insert a node before a reference node */
static int op_insert_before(const unsigned char *payload, uint32_t plen,
                            unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8 + 8 + 8) return 0;
    
    /* Payload: <<DocId:64, ParentHandle:64, NewNodeHandle:64, RefNodeHandle:64>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t parent_handle = read_uint64(payload + 8);
    uint64_t new_node_handle = read_uint64(payload + 16);
    uint64_t ref_node_handle = read_uint64(payload + 24);
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    lxb_dom_node_t *parent = (lxb_dom_node_t *)(uintptr_t)parent_handle;
    lxb_dom_node_t *new_node = (lxb_dom_node_t *)(uintptr_t)new_node_handle;
    lxb_dom_node_t *ref_node = (lxb_dom_node_t *)(uintptr_t)ref_node_handle;
    
    /* Verify all nodes belong to document */
    if (parent->owner_document != lxb_dom_interface_document(d->doc) ||
        new_node->owner_document != lxb_dom_interface_document(d->doc) ||
        ref_node->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    /* Verify ref_node is actually a child of parent */
    if (ref_node->parent != parent) {
        return error_response("ref_not_child", out, outlen);
    }
    
    /* Insert before */
    lxb_dom_node_insert_before(ref_node, new_node);
    
    /* Success */
    unsigned char *buf = (unsigned char*)malloc(1);
    if (!buf) return 0;
    buf[0] = 0;
    *out = buf;
    *outlen = 1;
    return 1;
}

/* REMOVE_NODE: Remove a node from its parent */
static int op_remove_node(const unsigned char *payload, uint32_t plen,
                          unsigned char **out, uint32_t *outlen) {
    if (plen < 8 + 8) return 0;
    
    /* Payload: <<DocId:64, NodeHandle:64>> */
    uint64_t doc_id = read_uint64(payload);
    uint64_t node_handle = read_uint64(payload + 8);
    
    Doc *d = find_doc(doc_id);
    if (!d) {
        return error_response("doc_not_found", out, outlen);
    }
    
    lxb_dom_node_t *node = (lxb_dom_node_t *)(uintptr_t)node_handle;
    
    /* Verify node belongs to document */
    if (node->owner_document != lxb_dom_interface_document(d->doc)) {
        return error_response("invalid_node", out, outlen);
    }
    
    /* Remove from parent */
    lxb_dom_node_remove(node);
    
    /* Note: We don't destroy the node here so it can potentially be reinserted */
    
    /* Success */
    unsigned char *buf = (unsigned char*)malloc(1);
    if (!buf) return 0;
    buf[0] = 0;
    *out = buf;
    *outlen = 1;
    return 1;
}

/* ---------- streaming parser operations ---------- */

// Operation: PARSE_STREAM_BEGIN
static int op_parse_stream_begin(unsigned char **out, uint32_t *outlen) {
    lxb_html_document_t *doc = lxb_html_document_create();
    if (!doc) return 0;
    
    // Initialize for chunked parsing
    lxb_status_t st = lxb_html_document_parse_chunk_begin(doc);
    if (st != LXB_STATUS_OK) {
        lxb_html_document_destroy(doc);
        return error_response("Parse chunk begin failed", out, outlen);
    }
    
    ParseSession *session = add_session(doc);
    if (!session) {
        lxb_html_document_destroy(doc);
        return error_response("Session creation failed", out, outlen);
    }
    
    session->begun = 1;
    session->ended = 0;
    session->chunks_processed = 0;
    
    // Return: <<0:8, SessionId:64>>
    unsigned char *buf = malloc(9);
    if (!buf) return 0;
    buf[0] = 0;  // success
    write_uint64(buf + 1, session->session_id);
    
    *out = buf;
    *outlen = 9;
    return 1;
}

// Operation: PARSE_STREAM_CHUNK
static int op_parse_stream_chunk(const unsigned char *payload, uint32_t plen,
                                  unsigned char **out, uint32_t *outlen) {
    if (plen < 8) return 0;
    
    // Extract SessionId
    uint64_t sid = read_uint64(payload);
    const unsigned char *chunk = payload + 8;
    uint32_t chunk_len = plen - 8;
    
    ParseSession *session = find_session(sid);
    if (!session || !session->begun || session->ended) {
        return error_response("Invalid session", out, outlen);
    }
    
    // Parse this chunk
    lxb_status_t st = lxb_html_document_parse_chunk(session->doc, 
                                                     (const lxb_char_t*)chunk, 
                                                     (size_t)chunk_len);
    if (st != LXB_STATUS_OK) {
        return error_response("Parse chunk failed", out, outlen);
    }
    
    session->chunks_processed++;
    
    // Success
    unsigned char *buf = malloc(1);
    if (!buf) return 0;
    buf[0] = 0;  // success
    *out = buf;
    *outlen = 1;
    return 1;
}

// Operation: PARSE_STREAM_END
static int op_parse_stream_end(const unsigned char *payload, uint32_t plen,
                                unsigned char **out, uint32_t *outlen) {
    if (plen != 8) return 0;
    
    uint64_t sid = read_uint64(payload);
    
    ParseSession *session = find_session(sid);
    if (!session || !session->begun || session->ended) {
        return error_response("Invalid session", out, outlen);
    }
    
    // Finalize parsing
    lxb_status_t st = lxb_html_document_parse_chunk_end(session->doc);
    if (st != LXB_STATUS_OK) {
        return error_response("Parse chunk end failed", out, outlen);
    }
    
    session->ended = 1;
    
    // Move document to document registry
    Doc *doc_entry = add_doc(session->doc);
    if (!doc_entry) {
        return error_response("Document registration failed", out, outlen);
    }
    
    // Remove session (document now owned by doc registry)
    remove_session(sid);
    
    // Return: <<0:8, DocId:64>>
    unsigned char *buf = malloc(9);
    if (!buf) return 0;
    buf[0] = 0;  // success
    write_uint64(buf + 1, doc_entry->id);
    
    *out = buf;
    *outlen = 9;
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
        else if(tag_eq(tag,"GET_ATTRIBUTE")){
            if(!op_get_attribute(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"SET_ATTRIBUTE")){
            if(!op_set_attribute(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"REMOVE_ATTRIBUTE")){
            if(!op_remove_attribute(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"GET_TEXT")){
            if(!op_get_text(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"SET_TEXT")){
            if(!op_set_text(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"INNER_HTML")){
            if(!op_inner_html(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"SET_INNER_HTML")){
            if(!op_set_inner_html(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"SERIALIZE_DOC")){
            if(!op_serialize_doc(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"CREATE_ELEMENT")){
            if(!op_create_element(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"APPEND_CHILD")){
            if(!op_append_child(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"INSERT_BEFORE")){
            if(!op_insert_before(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"REMOVE_NODE")){
            if(!op_remove_node(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"PARSE_STREAM_BEGIN")){
            if(!op_parse_stream_begin(&resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"PARSE_STREAM_CHUNK")){
            if(!op_parse_stream_chunk(payload, plen, &resp, &rplen)){ 
                free(req); break; 
            }
        }
        else if(tag_eq(tag,"PARSE_STREAM_END")){
            if(!op_parse_stream_end(payload, plen, &resp, &rplen)){ 
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
