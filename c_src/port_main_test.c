#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Lexbor */
#include <lexbor/html/parser.h>
#include <lexbor/dom/interfaces/document.h>
#include <lexbor/dom/interfaces/element.h>
#include <lexbor/dom/interfaces/node.h>
#include <lexbor/html/serialize.h>
#include <lexbor/selectors/selectors.h>
#include <lexbor/css/css.h>

/* ========================================================================
 * Copy utility structures and functions from port_main.c for testing
 * ======================================================================== */

/* Document registry */
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
            if(i+1<docs_len){
                memmove(&docs[i],&docs[i+1],(docs_len-i-1)*sizeof(Doc));
            }
            docs_len--;
            return 1;
        }
    }
    return 0;
}

/* Tag comparison */
static int tag_eq(const unsigned char tag[16], const char *name){
    size_t n=strlen(name); 
    if(n>16) n=16;
    if(memcmp(tag, name, n)!=0) return 0;
    for(size_t i=n;i<16;i++) if(tag[i]!=0) return 0;
    return 1;
}

/* Buffer utilities */
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

/* Packet framing utilities */
static void write_uint32_be(unsigned char *buf, uint32_t val) {
    buf[0] = (val >> 24) & 0xFF;
    buf[1] = (val >> 16) & 0xFF;
    buf[2] = (val >> 8) & 0xFF;
    buf[3] = val & 0xFF;
}

static uint32_t read_uint32_be(const unsigned char *buf) {
    return (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
}

static void write_uint64_be(unsigned char *buf, uint64_t val) {
    buf[0] = (val >> 56) & 0xFF;
    buf[1] = (val >> 48) & 0xFF;
    buf[2] = (val >> 40) & 0xFF;
    buf[3] = (val >> 32) & 0xFF;
    buf[4] = (val >> 24) & 0xFF;
    buf[5] = (val >> 16) & 0xFF;
    buf[6] = (val >> 8) & 0xFF;
    buf[7] = val & 0xFF;
}

static uint64_t read_uint64_be(const unsigned char *buf) {
    return ((uint64_t)buf[0] << 56) | ((uint64_t)buf[1] << 48) |
           ((uint64_t)buf[2] << 40) | ((uint64_t)buf[3] << 32) |
           ((uint64_t)buf[4] << 24) | ((uint64_t)buf[5] << 16) |
           ((uint64_t)buf[6] << 8) | (uint64_t)buf[7];
}

/* Aliases for streaming tests */
static inline void write_uint64(unsigned char *buf, uint64_t val) {
    write_uint64_be(buf, val);
}

static inline uint64_t read_uint64(const unsigned char *buf) {
    return read_uint64_be(buf);
}

static inline void write_uint32(unsigned char *buf, uint32_t val) {
    write_uint32_be(buf, val);
}

static inline uint32_t read_uint32(const unsigned char *buf) {
    return read_uint32_be(buf);
}

/* Parse session registry for streaming tests */
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
            sessions[i] = sessions[sessions_len - 1];
            sessions_len--;
            return 1;
        }
    }
    return 0;
}

/* Error response helper */
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

/* Streaming parser operations */
static int op_parse_stream_begin(unsigned char **out, uint32_t *outlen) {
    lxb_html_document_t *doc = lxb_html_document_create();
    if (!doc) return 0;
    
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
    
    unsigned char *buf = malloc(9);
    if (!buf) return 0;
    buf[0] = 0;  // success
    write_uint64(buf + 1, session->session_id);
    
    *out = buf;
    *outlen = 9;
    return 1;
}

static int op_parse_stream_chunk(const unsigned char *payload, uint32_t plen,
                                  unsigned char **out, uint32_t *outlen) {
    if (plen < 8) return 0;
    
    uint64_t sid = read_uint64(payload);
    const unsigned char *chunk = payload + 8;
    uint32_t chunk_len = plen - 8;
    
    ParseSession *session = find_session(sid);
    if (!session || !session->begun || session->ended) {
        return error_response("Invalid session", out, outlen);
    }
    
    lxb_status_t st = lxb_html_document_parse_chunk(session->doc, 
                                                     (const lxb_char_t*)chunk, 
                                                     (size_t)chunk_len);
    if (st != LXB_STATUS_OK) {
        return error_response("Parse chunk failed", out, outlen);
    }
    
    session->chunks_processed++;
    
    unsigned char *buf = malloc(1);
    if (!buf) return 0;
    buf[0] = 0;  // success
    *out = buf;
    *outlen = 1;
    return 1;
}

static int op_parse_stream_end(const unsigned char *payload, uint32_t plen,
                                unsigned char **out, uint32_t *outlen) {
    if (plen != 8) return 0;
    
    uint64_t sid = read_uint64(payload);
    
    ParseSession *session = find_session(sid);
    if (!session || !session->begun || session->ended) {
        return error_response("Invalid session", out, outlen);
    }
    
    lxb_status_t st = lxb_html_document_parse_chunk_end(session->doc);
    if (st != LXB_STATUS_OK) {
        return error_response("Parse chunk end failed", out, outlen);
    }
    
    session->ended = 1;
    
    Doc *doc_entry = add_doc(session->doc);
    if (!doc_entry) {
        return error_response("Document registration failed", out, outlen);
    }
    
    remove_session(sid);
    
    unsigned char *buf = malloc(9);
    if (!buf) return 0;
    buf[0] = 0;  // success
    write_uint64(buf + 1, doc_entry->id);
    
    *out = buf;
    *outlen = 9;
    return 1;
}

/* ========================================================================
 * Test Helper Functions
 * ======================================================================== */

static int test_count = 0;
static int test_passed = 0;
static int test_failed = 0;

#define TEST(name) \
    static void test_##name(void); \
    static void run_test_##name(void) { \
        printf("Running test: %s ... ", #name); \
        fflush(stdout); \
        test_count++; \
        test_##name(); \
        test_passed++; \
        printf("PASS\n"); \
    } \
    static void test_##name(void)

#define ASSERT(cond) \
    do { \
        if (!(cond)) { \
            fprintf(stderr, "\nAssertion failed: %s at %s:%d\n", #cond, __FILE__, __LINE__); \
            test_failed++; \
            test_passed--; \
            return; \
        } \
    } while(0)

#define ASSERT_EQ(a, b) \
    do { \
        if ((a) != (b)) { \
            fprintf(stderr, "\nAssertion failed: %s == %s (got %ld vs %ld) at %s:%d\n", \
                    #a, #b, (long)(a), (long)(b), __FILE__, __LINE__); \
            test_failed++; \
            test_passed--; \
            return; \
        } \
    } while(0)

#define ASSERT_STR_EQ(a, b) \
    do { \
        if (strcmp((const char*)(a), (b)) != 0) { \
            fprintf(stderr, "\nAssertion failed: %s == %s (got '%s' vs '%s') at %s:%d\n", \
                    #a, #b, (const char*)(a), (b), __FILE__, __LINE__); \
            test_failed++; \
            test_passed--; \
            return; \
        } \
    } while(0)

#define RUN_TEST(name) run_test_##name()

/* ========================================================================
 * Unit Tests
 * ======================================================================== */

/* Test tag_eq function */
TEST(tag_eq_exact_match) {
    unsigned char tag[16] = {0};
    memcpy(tag, "div", 3);
    ASSERT(tag_eq(tag, "div") == 1);
}

TEST(tag_eq_no_match) {
    unsigned char tag[16] = {0};
    memcpy(tag, "div", 3);
    ASSERT(tag_eq(tag, "span") == 0);
}

TEST(tag_eq_partial_match) {
    unsigned char tag[16] = {0};
    memcpy(tag, "divspan", 7);
    ASSERT(tag_eq(tag, "div") == 0);  // Should not match partial
}

TEST(tag_eq_empty) {
    unsigned char tag[16] = {0};
    ASSERT(tag_eq(tag, "") == 1);  // Empty tag matches empty string
}

TEST(tag_eq_long_name) {
    unsigned char tag[16] = {0};
    // Copy exactly 16 chars from a longer string
    memcpy(tag, "verylongelementname", 16);  // Gets "verylongelement" + "n" = 16 chars
    // First 16 chars of "verylongelementname" are "verylongelementn"
    ASSERT(tag_eq(tag, "verylongelementn") == 1);
}

/* Test sbuf_reserve function */
TEST(sbuf_reserve_initial) {
    sbuf_t buf = {NULL, 0, 0};
    ASSERT(sbuf_reserve(&buf, 100) == 1);
    ASSERT(buf.cap >= 100);
    ASSERT(buf.ptr != NULL);
    free(buf.ptr);
}

TEST(sbuf_reserve_no_realloc_needed) {
    sbuf_t buf = {NULL, 0, 0};
    sbuf_reserve(&buf, 100);
    size_t old_cap = buf.cap;
    unsigned char *old_ptr = buf.ptr;
    
    ASSERT(sbuf_reserve(&buf, 50) == 1);
    ASSERT(buf.cap == old_cap);
    ASSERT(buf.ptr == old_ptr);
    
    free(buf.ptr);
}

TEST(sbuf_reserve_realloc_needed) {
    sbuf_t buf = {NULL, 0, 0};
    sbuf_reserve(&buf, 100);
    buf.len = 90;
    
    ASSERT(sbuf_reserve(&buf, 50) == 1);
    ASSERT(buf.cap >= 140);
    
    free(buf.ptr);
}

TEST(sbuf_reserve_multiple_calls) {
    sbuf_t buf = {NULL, 0, 0};
    
    ASSERT(sbuf_reserve(&buf, 10) == 1);
    ASSERT(buf.cap >= 10);
    
    ASSERT(sbuf_reserve(&buf, 100) == 1);
    ASSERT(buf.cap >= 100);
    
    ASSERT(sbuf_reserve(&buf, 1000) == 1);
    ASSERT(buf.cap >= 1000);
    
    free(buf.ptr);
}

/* Test sink_cb function */
TEST(sink_cb_basic) {
    sbuf_t buf = {NULL, 0, 0};
    const char *data = "Hello, World!";
    size_t len = strlen(data);
    
    lxb_status_t status = sink_cb((const lxb_char_t*)data, len, &buf);
    
    ASSERT(status == LXB_STATUS_OK);
    ASSERT(buf.len == len);
    ASSERT(memcmp(buf.ptr, data, len) == 0);
    
    free(buf.ptr);
}

TEST(sink_cb_multiple_calls) {
    sbuf_t buf = {NULL, 0, 0};
    
    sink_cb((const lxb_char_t*)"Hello", 5, &buf);
    ASSERT(buf.len == 5);
    
    sink_cb((const lxb_char_t*)" World", 6, &buf);
    ASSERT(buf.len == 11);
    ASSERT(memcmp(buf.ptr, "Hello World", 11) == 0);
    
    free(buf.ptr);
}

/* Test uint32 encoding/decoding */
TEST(uint32_encoding) {
    unsigned char buf[4];
    uint32_t val = 0x12345678;
    
    write_uint32_be(buf, val);
    
    ASSERT_EQ(buf[0], 0x12);
    ASSERT_EQ(buf[1], 0x34);
    ASSERT_EQ(buf[2], 0x56);
    ASSERT_EQ(buf[3], 0x78);
}

TEST(uint32_decoding) {
    unsigned char buf[4] = {0x12, 0x34, 0x56, 0x78};
    uint32_t val = read_uint32_be(buf);
    
    ASSERT_EQ(val, 0x12345678);
}

TEST(uint32_roundtrip) {
    unsigned char buf[4];
    uint32_t original = 0xDEADBEEF;
    
    write_uint32_be(buf, original);
    uint32_t decoded = read_uint32_be(buf);
    
    ASSERT_EQ(decoded, original);
}

/* Test uint64 encoding/decoding */
TEST(uint64_encoding) {
    unsigned char buf[8];
    uint64_t val = 0x123456789ABCDEF0ULL;
    
    write_uint64_be(buf, val);
    
    ASSERT_EQ(buf[0], 0x12);
    ASSERT_EQ(buf[1], 0x34);
    ASSERT_EQ(buf[2], 0x56);
    ASSERT_EQ(buf[3], 0x78);
    ASSERT_EQ(buf[4], 0x9A);
    ASSERT_EQ(buf[5], 0xBC);
    ASSERT_EQ(buf[6], 0xDE);
    ASSERT_EQ(buf[7], 0xF0);
}

TEST(uint64_decoding) {
    unsigned char buf[8] = {0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0};
    uint64_t val = read_uint64_be(buf);
    
    ASSERT_EQ(val, 0x123456789ABCDEF0ULL);
}

TEST(uint64_roundtrip) {
    unsigned char buf[8];
    uint64_t original = 0xFEDCBA9876543210ULL;
    
    write_uint64_be(buf, original);
    uint64_t decoded = read_uint64_be(buf);
    
    ASSERT_EQ(decoded, original);
}

/* Test document registry */
TEST(doc_registry_add_single) {
    // Reset registry
    if (docs) {
        for (size_t i = 0; i < docs_len; i++) {
            lxb_html_document_destroy(docs[i].doc);
        }
        free(docs);
        docs = NULL;
        docs_len = 0;
        docs_cap = 0;
        next_id = 1;
    }
    
    lxb_html_document_t *doc = lxb_html_document_create();
    ASSERT(doc != NULL);
    
    Doc *d = add_doc(doc);
    ASSERT(d != NULL);
    ASSERT_EQ(d->id, 1);
    ASSERT(d->doc == doc);
    ASSERT_EQ(docs_len, 1);
}

TEST(doc_registry_add_multiple) {
    // Reset registry
    if (docs) {
        for (size_t i = 0; i < docs_len; i++) {
            lxb_html_document_destroy(docs[i].doc);
        }
        free(docs);
        docs = NULL;
        docs_len = 0;
        docs_cap = 0;
        next_id = 1;
    }
    
    lxb_html_document_t *doc1 = lxb_html_document_create();
    lxb_html_document_t *doc2 = lxb_html_document_create();
    lxb_html_document_t *doc3 = lxb_html_document_create();
    
    Doc *d1 = add_doc(doc1);
    Doc *d2 = add_doc(doc2);
    Doc *d3 = add_doc(doc3);
    
    ASSERT(d1 != NULL && d2 != NULL && d3 != NULL);
    ASSERT_EQ(d1->id, 1);
    ASSERT_EQ(d2->id, 2);
    ASSERT_EQ(d3->id, 3);
    ASSERT_EQ(docs_len, 3);
}

TEST(doc_registry_find_existing) {
    // Reset registry
    if (docs) {
        for (size_t i = 0; i < docs_len; i++) {
            lxb_html_document_destroy(docs[i].doc);
        }
        free(docs);
        docs = NULL;
        docs_len = 0;
        docs_cap = 0;
        next_id = 1;
    }
    
    lxb_html_document_t *doc = lxb_html_document_create();
    Doc *d = add_doc(doc);
    uint64_t doc_id = d->id;
    
    Doc *found = find_doc(doc_id);
    ASSERT(found != NULL);
    ASSERT_EQ(found->id, doc_id);
    ASSERT(found->doc == doc);
}

TEST(doc_registry_find_nonexistent) {
    // Reset registry
    if (docs) {
        for (size_t i = 0; i < docs_len; i++) {
            lxb_html_document_destroy(docs[i].doc);
        }
        free(docs);
        docs = NULL;
        docs_len = 0;
        docs_cap = 0;
        next_id = 1;
    }
    
    Doc *found = find_doc(999999);
    ASSERT(found == NULL);
}

TEST(doc_registry_remove) {
    // Reset registry
    if (docs) {
        for (size_t i = 0; i < docs_len; i++) {
            lxb_html_document_destroy(docs[i].doc);
        }
        free(docs);
        docs = NULL;
        docs_len = 0;
        docs_cap = 0;
        next_id = 1;
    }
    
    lxb_html_document_t *doc1 = lxb_html_document_create();
    lxb_html_document_t *doc2 = lxb_html_document_create();
    
    Doc *d1 = add_doc(doc1);
    Doc *d2 = add_doc(doc2);
    uint64_t id1 = d1->id;
    uint64_t id2 = d2->id;
    
    ASSERT_EQ(docs_len, 2);
    
    int removed = remove_doc(id1);
    ASSERT_EQ(removed, 1);
    ASSERT_EQ(docs_len, 1);
    
    Doc *found1 = find_doc(id1);
    Doc *found2 = find_doc(id2);
    
    ASSERT(found1 == NULL);
    ASSERT(found2 != NULL);
}

TEST(doc_registry_remove_nonexistent) {
    // Reset registry
    if (docs) {
        for (size_t i = 0; i < docs_len; i++) {
            lxb_html_document_destroy(docs[i].doc);
        }
        free(docs);
        docs = NULL;
        docs_len = 0;
        docs_cap = 0;
        next_id = 1;
    }
    
    int removed = remove_doc(999999);
    ASSERT_EQ(removed, 0);
}

/* Test HTML parsing and serialization */
TEST(html_parse_simple) {
    lxb_html_document_t *doc = lxb_html_document_create();
    ASSERT(doc != NULL);
    
    const char *html = "<div>Hello</div>";
    lxb_status_t status = lxb_html_document_parse(doc, 
        (const lxb_char_t*)html, strlen(html));
    
    ASSERT(status == LXB_STATUS_OK);
    
    lxb_html_document_destroy(doc);
}

TEST(html_parse_nested) {
    lxb_html_document_t *doc = lxb_html_document_create();
    ASSERT(doc != NULL);
    
    const char *html = "<div><p>Nested <span>content</span></p></div>";
    lxb_status_t status = lxb_html_document_parse(doc,
        (const lxb_char_t*)html, strlen(html));
    
    ASSERT(status == LXB_STATUS_OK);
    
    lxb_html_document_destroy(doc);
}

TEST(html_parse_malformed) {
    lxb_html_document_t *doc = lxb_html_document_create();
    ASSERT(doc != NULL);
    
    // Malformed HTML should still parse (HTML5 parser is forgiving)
    const char *html = "<div><p>Unclosed tags";
    lxb_status_t status = lxb_html_document_parse(doc,
        (const lxb_char_t*)html, strlen(html));
    
    ASSERT(status == LXB_STATUS_OK);
    
    lxb_html_document_destroy(doc);
}

TEST(html_serialize_simple) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<div id='test'>Content</div>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    sbuf_t buf = {NULL, 0, 0};
    
    lxb_status_t status = lxb_html_serialize_tree_cb(
        lxb_dom_interface_node(doc), 
        sink_cb, 
        &buf
    );
    
    ASSERT(status == LXB_STATUS_OK);
    ASSERT(buf.len > 0);
    ASSERT(buf.ptr != NULL);
    
    // Check that serialized content contains our div
    ASSERT(memmem(buf.ptr, buf.len, "id=\"test\"", 9) != NULL);
    ASSERT(memmem(buf.ptr, buf.len, "Content", 7) != NULL);
    
    free(buf.ptr);
    lxb_html_document_destroy(doc);
}

/* ========================================================================
 * DOM Manipulation Tests
 * ======================================================================== */

/* Test get_attribute */
TEST(dom_get_attribute) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<a href='/test' class='link' id='main'>Link</a>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *link_node = body->first_child;
    ASSERT(link_node != NULL);
    lxb_dom_element_t *link = lxb_dom_interface_element(link_node);
    
    // Get href attribute
    size_t attr_len;
    const lxb_char_t *href = lxb_dom_element_get_attribute(link, 
        (const lxb_char_t*)"href", 4, &attr_len);
    ASSERT(href != NULL);
    ASSERT_EQ(attr_len, 5);
    ASSERT(memcmp(href, "/test", 5) == 0);
    
    // Get class attribute
    const lxb_char_t *cls = lxb_dom_element_get_attribute(link,
        (const lxb_char_t*)"class", 5, &attr_len);
    ASSERT(cls != NULL);
    ASSERT_EQ(attr_len, 4);
    ASSERT(memcmp(cls, "link", 4) == 0);
    
    // Get non-existent attribute
    const lxb_char_t *missing = lxb_dom_element_get_attribute(link,
        (const lxb_char_t*)"data-test", 9, &attr_len);
    ASSERT(missing == NULL);
    
    lxb_html_document_destroy(doc);
}

/* Test set_attribute */
TEST(dom_set_attribute) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<div id='test'>Content</div>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *div_node = body->first_child;
    ASSERT(div_node != NULL);
    lxb_dom_element_t *div = lxb_dom_interface_element(div_node);
    
    // Set new attribute
    lxb_dom_attr_t *attr = lxb_dom_element_set_attribute(div,
        (const lxb_char_t*)"class", 5,
        (const lxb_char_t*)"active", 6);
    ASSERT(attr != NULL);
    
    // Verify attribute was set
    size_t attr_len;
    const lxb_char_t *cls = lxb_dom_element_get_attribute(div,
        (const lxb_char_t*)"class", 5, &attr_len);
    ASSERT(cls != NULL);
    ASSERT_EQ(attr_len, 6);
    ASSERT(memcmp(cls, "active", 6) == 0);
    
    // Update existing attribute
    attr = lxb_dom_element_set_attribute(div,
        (const lxb_char_t*)"id", 2,
        (const lxb_char_t*)"updated", 7);
    ASSERT(attr != NULL);
    
    const lxb_char_t *id = lxb_dom_element_get_attribute(div,
        (const lxb_char_t*)"id", 2, &attr_len);
    ASSERT(id != NULL);
    ASSERT_EQ(attr_len, 7);
    ASSERT(memcmp(id, "updated", 7) == 0);
    
    lxb_html_document_destroy(doc);
}

/* Test remove_attribute */
TEST(dom_remove_attribute) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<div id='test' class='active' data-value='123'>Content</div>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *div_node = body->first_child;
    ASSERT(div_node != NULL);
    lxb_dom_element_t *div = lxb_dom_interface_element(div_node);
    
    // Verify attribute exists
    size_t attr_len;
    const lxb_char_t *cls = lxb_dom_element_get_attribute(div,
        (const lxb_char_t*)"class", 5, &attr_len);
    ASSERT(cls != NULL);
    
    // Remove attribute
    lxb_status_t status = lxb_dom_element_remove_attribute(div,
        (const lxb_char_t*)"class", 5);
    ASSERT(status == LXB_STATUS_OK);
    
    // Verify attribute is gone
    cls = lxb_dom_element_get_attribute(div,
        (const lxb_char_t*)"class", 5, &attr_len);
    ASSERT(cls == NULL);
    
    // Other attributes should still exist
    const lxb_char_t *id = lxb_dom_element_get_attribute(div,
        (const lxb_char_t*)"id", 2, &attr_len);
    ASSERT(id != NULL);
    
    lxb_html_document_destroy(doc);
}

/* Test get_text */
TEST(dom_get_text) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<div>Hello <b>World</b>!</div>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *div_node = body->first_child;
    ASSERT(div_node != NULL);
    lxb_dom_element_t *div = lxb_dom_interface_element(div_node);
    
    // Get text content (includes text from child elements)
    size_t text_len;
    const lxb_char_t *text = lxb_dom_node_text_content(
        lxb_dom_interface_node(div), &text_len);
    ASSERT(text != NULL);
    ASSERT(text_len > 0);
    ASSERT(memcmp(text, "Hello World!", 12) == 0);
    
    lxb_html_document_destroy(doc);
}

/* Test set_text */
TEST(dom_set_text) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<div>Old <b>content</b></div>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *div_node = body->first_child;
    ASSERT(div_node != NULL);
    lxb_dom_element_t *div = lxb_dom_interface_element(div_node);
    
    // Set new text content
    const char *new_text = "New text";
    lxb_status_t status = lxb_dom_node_text_content_set(
        lxb_dom_interface_node(div),
        (const lxb_char_t*)new_text, strlen(new_text));
    ASSERT(status == LXB_STATUS_OK);
    
    // Verify text was updated
    size_t text_len;
    const lxb_char_t *text = lxb_dom_node_text_content(
        lxb_dom_interface_node(div), &text_len);
    ASSERT(text != NULL);
    ASSERT_EQ(text_len, 8);
    ASSERT(memcmp(text, "New text", 8) == 0);
    
    lxb_html_document_destroy(doc);
}

/* Test inner_html (via serialization) */
TEST(dom_inner_html) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<div><p>First</p><p>Second</p></div>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *div_node = body->first_child;
    ASSERT(div_node != NULL);
    lxb_dom_element_t *div = lxb_dom_interface_element(div_node);
    
    // Serialize child nodes
    sbuf_t buf = {NULL, 0, 0};
    lxb_dom_node_t *child = lxb_dom_interface_node(div)->first_child;
    while (child) {
        lxb_status_t status = lxb_html_serialize_tree_cb(child, sink_cb, &buf);
        ASSERT(status == LXB_STATUS_OK);
        child = child->next;
    }
    
    ASSERT(buf.len > 0);
    ASSERT(memmem(buf.ptr, buf.len, "<p>First</p>", 12) != NULL);
    ASSERT(memmem(buf.ptr, buf.len, "<p>Second</p>", 13) != NULL);
    
    free(buf.ptr);
    lxb_html_document_destroy(doc);
}

/* Test create_element */
TEST(dom_create_element) {
    lxb_html_document_t *doc = lxb_html_document_create();
    lxb_html_document_parse(doc, (const lxb_char_t*)"<html><body></body></html>", 26);
    
    // Create new element
    lxb_dom_element_t *div = lxb_dom_document_create_element(
        lxb_dom_interface_document(doc),
        (const lxb_char_t*)"div", 3, NULL);
    ASSERT(div != NULL);
    
    // Verify element type - just check it's not null
    lxb_dom_node_t *node = lxb_dom_interface_node(div);
    ASSERT(node != NULL);
    ASSERT(node->local_name > 0);
    
    // Create another element type
    lxb_dom_element_t *span = lxb_dom_document_create_element(
        lxb_dom_interface_document(doc),
        (const lxb_char_t*)"span", 4, NULL);
    ASSERT(span != NULL);
    
    lxb_html_document_destroy(doc);
}

/* Test append_child */
TEST(dom_append_child) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<ul id='list'></ul>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *ul_node = body->first_child;
    ASSERT(ul_node != NULL);
    lxb_dom_element_t *ul = lxb_dom_interface_element(ul_node);
    
    // Create and append first item
    lxb_dom_element_t *li1 = lxb_dom_document_create_element(
        lxb_dom_interface_document(doc),
        (const lxb_char_t*)"li", 2, NULL);
    ASSERT(li1 != NULL);
    
    lxb_dom_node_insert_child(lxb_dom_interface_node(ul),
                              lxb_dom_interface_node(li1));
    
    // Verify child was appended
    lxb_dom_node_t *first_child = lxb_dom_interface_node(ul)->first_child;
    ASSERT(first_child == lxb_dom_interface_node(li1));
    
    // Append second item
    lxb_dom_element_t *li2 = lxb_dom_document_create_element(
        lxb_dom_interface_document(doc),
        (const lxb_char_t*)"li", 2, NULL);
    lxb_dom_node_insert_child(lxb_dom_interface_node(ul),
                              lxb_dom_interface_node(li2));
    
    // Verify second child
    ASSERT(lxb_dom_interface_node(li1)->next == lxb_dom_interface_node(li2));
    
    lxb_html_document_destroy(doc);
}

/* Test insert_before */
TEST(dom_insert_before) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<ul><li id='second'>Second</li></ul>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *ul_node = body->first_child;
    ASSERT(ul_node != NULL);
    lxb_dom_element_t *ul = lxb_dom_interface_element(ul_node);
    lxb_dom_node_t *second_node = ul_node->first_child;
    ASSERT(second_node != NULL);
    lxb_dom_element_t *second = lxb_dom_interface_element(second_node);
    
    // Create first item
    lxb_dom_element_t *first = lxb_dom_document_create_element(
        lxb_dom_interface_document(doc),
        (const lxb_char_t*)"li", 2, NULL);
    ASSERT(first != NULL);
    
    // Insert before second
    lxb_dom_node_insert_before(lxb_dom_interface_node(second),
                               lxb_dom_interface_node(first));
    
    // Verify order
    lxb_dom_node_t *first_child = lxb_dom_interface_node(ul)->first_child;
    ASSERT(first_child == lxb_dom_interface_node(first));
    ASSERT(first_child->next == lxb_dom_interface_node(second));
    
    lxb_html_document_destroy(doc);
}

/* Test remove_node */
TEST(dom_remove_node) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<div><p id='remove'>Remove</p><p id='keep'>Keep</p></div>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *div_node = body->first_child;
    ASSERT(div_node != NULL);
    lxb_dom_element_t *div = lxb_dom_interface_element(div_node);
    lxb_dom_node_t *remove_p_node = div_node->first_child;
    ASSERT(remove_p_node != NULL);
    lxb_dom_element_t *remove_p = lxb_dom_interface_element(remove_p_node);
    lxb_dom_node_t *keep_p_node = remove_p_node->next;
    ASSERT(keep_p_node != NULL);
    lxb_dom_element_t *keep_p = lxb_dom_interface_element(keep_p_node);
    
    // Remove first paragraph
    lxb_dom_node_remove(remove_p_node);
    
    // Verify it's gone and second remains
    lxb_dom_node_t *first_node = div_node->first_child;
    ASSERT(first_node == keep_p_node);
    ASSERT(first_node->next == NULL);
    
    lxb_html_document_destroy(doc);
}

/* Test complex DOM building */
TEST(dom_complex_building) {
    lxb_html_document_t *doc = lxb_html_document_create();
    const char *html = "<div id='container'></div>";
    lxb_html_document_parse(doc, (const lxb_char_t*)html, strlen(html));
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc->body);
    lxb_dom_node_t *container_node = body->first_child;
    ASSERT(container_node != NULL);
    lxb_dom_element_t *container = lxb_dom_interface_element(container_node);
    
    // Create header
    lxb_dom_element_t *h1 = lxb_dom_document_create_element(
        lxb_dom_interface_document(doc),
        (const lxb_char_t*)"h1", 2, NULL);
    lxb_dom_element_set_attribute(h1,
        (const lxb_char_t*)"class", 5,
        (const lxb_char_t*)"title", 5);
    lxb_dom_node_text_content_set(lxb_dom_interface_node(h1),
        (const lxb_char_t*)"Page Title", 10);
    lxb_dom_node_insert_child(lxb_dom_interface_node(container),
                              lxb_dom_interface_node(h1));
    
    // Create section
    lxb_dom_element_t *section = lxb_dom_document_create_element(
        lxb_dom_interface_document(doc),
        (const lxb_char_t*)"section", 7, NULL);
    lxb_dom_node_insert_child(lxb_dom_interface_node(container),
                              lxb_dom_interface_node(section));
    
    // Create paragraph
    lxb_dom_element_t *p = lxb_dom_document_create_element(
        lxb_dom_interface_document(doc),
        (const lxb_char_t*)"p", 1, NULL);
    lxb_dom_node_text_content_set(lxb_dom_interface_node(p),
        (const lxb_char_t*)"Content", 7);
    lxb_dom_node_insert_child(lxb_dom_interface_node(section),
                              lxb_dom_interface_node(p));
    
    // Verify structure
    ASSERT(container_node->first_child == lxb_dom_interface_node(h1));
    ASSERT(lxb_dom_interface_node(h1)->next == lxb_dom_interface_node(section));
    ASSERT(lxb_dom_interface_node(section)->first_child == lxb_dom_interface_node(p));
    
    // Verify serialization
    sbuf_t buf = {NULL, 0, 0};
    lxb_status_t status = lxb_html_serialize_tree_cb(
        lxb_dom_interface_node(doc), sink_cb, &buf);
    ASSERT(status == LXB_STATUS_OK);
    ASSERT(memmem(buf.ptr, buf.len, "Page Title", 10) != NULL);
    ASSERT(memmem(buf.ptr, buf.len, "Content", 7) != NULL);
    
    free(buf.ptr);
    lxb_html_document_destroy(doc);
}

/* ========================================================================
 * Streaming Parser Tests
 * ======================================================================== */

TEST(stream_begin_end_basic) {
    // Test basic begin/end sequence
    unsigned char *out = NULL;
    uint32_t outlen = 0;
    
    // Begin streaming
    int result = op_parse_stream_begin(&out, &outlen);
    ASSERT(result == 1);
    ASSERT(out != NULL);
    ASSERT(outlen == 9);
    ASSERT(out[0] == 0);  // success marker
    
    // Extract session ID
    uint64_t session_id = read_uint64(out + 1);
    ASSERT(session_id > 0);
    
    free(out);
    out = NULL;
    
    // Feed a complete HTML chunk
    const char *html = "<html><body><p>Test</p></body></html>";
    uint32_t html_len = strlen(html);
    uint32_t chunk_payload_len = 8 + html_len;
    unsigned char *chunk_payload = malloc(chunk_payload_len);
    write_uint64(chunk_payload, session_id);
    memcpy(chunk_payload + 8, html, html_len);
    
    result = op_parse_stream_chunk(chunk_payload, chunk_payload_len, &out, &outlen);
    ASSERT(result == 1);
    ASSERT(out != NULL);
    ASSERT(outlen == 1);
    ASSERT(out[0] == 0);  // success
    
    free(chunk_payload);
    free(out);
    out = NULL;
    
    // End streaming
    unsigned char end_payload[8];
    write_uint64(end_payload, session_id);
    
    result = op_parse_stream_end(end_payload, 8, &out, &outlen);
    ASSERT(result == 1);
    ASSERT(out != NULL);
    ASSERT(outlen == 9);
    ASSERT(out[0] == 0);  // success
    
    uint64_t doc_id = read_uint64(out + 1);
    ASSERT(doc_id > 0);
    
    free(out);
    
    // Verify document was registered and contains our content
    Doc *doc_entry = find_doc(doc_id);
    ASSERT(doc_entry != NULL);
    ASSERT(doc_entry->doc != NULL);
    
    // Cleanup
    remove_doc(doc_id);
}

TEST(stream_multiple_chunks) {
    // Test streaming with multiple chunks
    unsigned char *out = NULL;
    uint32_t outlen = 0;
    
    // Begin
    int result = op_parse_stream_begin(&out, &outlen);
    ASSERT(result == 1);
    uint64_t session_id = read_uint64(out + 1);
    free(out);
    
    // Feed HTML in 3 chunks
    const char *chunks[] = {
        "<html><body>",
        "<p>Chunk 1</p><p>Chunk 2</p>",
        "</body></html>"
    };
    
    for (int i = 0; i < 3; i++) {
        uint32_t chunk_len = strlen(chunks[i]);
        uint32_t payload_len = 8 + chunk_len;
        unsigned char *payload = malloc(payload_len);
        write_uint64(payload, session_id);
        memcpy(payload + 8, chunks[i], chunk_len);
        
        result = op_parse_stream_chunk(payload, payload_len, &out, &outlen);
        ASSERT(result == 1);
        ASSERT(out[0] == 0);
        
        free(payload);
        free(out);
    }
    
    // End
    unsigned char end_payload[8];
    write_uint64(end_payload, session_id);
    result = op_parse_stream_end(end_payload, 8, &out, &outlen);
    ASSERT(result == 1);
    
    uint64_t doc_id = read_uint64(out + 1);
    free(out);
    
    // Verify document
    Doc *doc_entry = find_doc(doc_id);
    ASSERT(doc_entry != NULL);
    
    // Check that both paragraphs are in the document
    lexbor_str_t str = {0};
    lxb_html_serialize_tree_str(lxb_dom_interface_node(doc_entry->doc), &str);
    
    ASSERT(memmem(str.data, str.length, "Chunk 1", 7) != NULL);
    ASSERT(memmem(str.data, str.length, "Chunk 2", 7) != NULL);
    
    lexbor_str_destroy(&str, doc_entry->doc->dom_document.text, false);
    remove_doc(doc_id);
}

TEST(stream_split_in_tag) {
    // Test streaming where chunk boundary splits a tag
    unsigned char *out = NULL;
    uint32_t outlen = 0;
    
    // Begin
    op_parse_stream_begin(&out, &outlen);
    uint64_t session_id = read_uint64(out + 1);
    free(out);
    
    // Feed HTML with split in middle of opening tag
    const char *chunks[] = {
        "<html><body><p cla",      // Split in attribute name
        "ss='test'>Content",        // Continue attribute and content
        "</p></body></html>"
    };
    
    for (int i = 0; i < 3; i++) {
        uint32_t chunk_len = strlen(chunks[i]);
        uint32_t payload_len = 8 + chunk_len;
        unsigned char *payload = malloc(payload_len);
        write_uint64(payload, session_id);
        memcpy(payload + 8, chunks[i], chunk_len);
        
        op_parse_stream_chunk(payload, payload_len, &out, &outlen);
        free(payload);
        free(out);
    }
    
    // End
    unsigned char end_payload[8];
    write_uint64(end_payload, session_id);
    op_parse_stream_end(end_payload, 8, &out, &outlen);
    uint64_t doc_id = read_uint64(out + 1);
    free(out);
    
    // Verify the attribute was parsed correctly
    Doc *doc_entry = find_doc(doc_id);
    ASSERT(doc_entry != NULL);
    
    lxb_dom_node_t *body = lxb_dom_interface_node(doc_entry->doc->body);
    lxb_dom_node_t *p_node = body->first_child;
    ASSERT(p_node != NULL);
    
    lxb_dom_element_t *p_elem = lxb_dom_interface_element(p_node);
    const lxb_char_t *class_val = lxb_dom_element_get_attribute(
        p_elem, (const lxb_char_t*)"class", 5, NULL);
    
    ASSERT(class_val != NULL);
    ASSERT(memcmp(class_val, "test", 4) == 0);
    
    remove_doc(doc_id);
}

TEST(stream_invalid_session_chunk) {
    // Test chunk operation with invalid session ID
    unsigned char *out = NULL;
    uint32_t outlen = 0;
    
    uint64_t invalid_session = 999999;
    const char *html = "<p>Test</p>";
    uint32_t payload_len = 8 + strlen(html);
    unsigned char *payload = malloc(payload_len);
    write_uint64(payload, invalid_session);
    memcpy(payload + 8, html, strlen(html));
    
    int result = op_parse_stream_chunk(payload, payload_len, &out, &outlen);
    
    // Should succeed (return 1) but indicate error in response
    ASSERT(result == 1);
    ASSERT(out != NULL);
    ASSERT(outlen >= 1);
    ASSERT(out[0] == 1);  // error marker
    
    free(payload);
    free(out);
}

TEST(stream_invalid_session_end) {
    // Test end operation with invalid session ID
    unsigned char *out = NULL;
    uint32_t outlen = 0;
    
    uint64_t invalid_session = 999999;
    unsigned char payload[8];
    write_uint64(payload, invalid_session);
    
    int result = op_parse_stream_end(payload, 8, &out, &outlen);
    
    // Should succeed (return 1) but indicate error in response
    ASSERT(result == 1);
    ASSERT(out != NULL);
    ASSERT(outlen >= 1);
    ASSERT(out[0] == 1);  // error marker
    
    free(out);
}

TEST(stream_reuse_after_end) {
    // Test that session cannot be reused after end
    unsigned char *out = NULL;
    uint32_t outlen = 0;
    
    // Begin and immediately end
    op_parse_stream_begin(&out, &outlen);
    uint64_t session_id = read_uint64(out + 1);
    free(out);
    
    unsigned char end_payload[8];
    write_uint64(end_payload, session_id);
    op_parse_stream_end(end_payload, 8, &out, &outlen);
    uint64_t doc_id = read_uint64(out + 1);
    free(out);
    
    // Try to use session after it's been ended
    const char *html = "<p>Test</p>";
    uint32_t payload_len = 8 + strlen(html);
    unsigned char *payload = malloc(payload_len);
    write_uint64(payload, session_id);
    memcpy(payload + 8, html, strlen(html));
    
    int result = op_parse_stream_chunk(payload, payload_len, &out, &outlen);
    ASSERT(result == 1);
    ASSERT(out[0] == 1);  // error - session already ended
    
    free(payload);
    free(out);
    remove_doc(doc_id);
}

TEST(stream_large_chunks) {
    // Test streaming with many small chunks
    unsigned char *out = NULL;
    uint32_t outlen = 0;
    
    // Begin
    op_parse_stream_begin(&out, &outlen);
    uint64_t session_id = read_uint64(out + 1);
    free(out);
    
    // Send opening
    const char *open = "<html><body><ul>";
    uint32_t open_len = strlen(open);
    unsigned char *open_payload = malloc(8 + open_len);
    write_uint64(open_payload, session_id);
    memcpy(open_payload + 8, open, open_len);
    op_parse_stream_chunk(open_payload, 8 + open_len, &out, &outlen);
    free(open_payload);
    free(out);
    
    // Send 50 list items as separate chunks
    for (int i = 1; i <= 50; i++) {
        char item[64];
        int item_len = snprintf(item, sizeof(item), "<li>Item %d</li>", i);
        
        unsigned char *item_payload = malloc(8 + item_len);
        write_uint64(item_payload, session_id);
        memcpy(item_payload + 8, item, item_len);
        
        op_parse_stream_chunk(item_payload, 8 + item_len, &out, &outlen);
        free(item_payload);
        free(out);
    }
    
    // Send closing
    const char *close = "</ul></body></html>";
    uint32_t close_len = strlen(close);
    unsigned char *close_payload = malloc(8 + close_len);
    write_uint64(close_payload, session_id);
    memcpy(close_payload + 8, close, close_len);
    op_parse_stream_chunk(close_payload, 8 + close_len, &out, &outlen);
    free(close_payload);
    free(out);
    
    // End
    unsigned char end_payload[8];
    write_uint64(end_payload, session_id);
    op_parse_stream_end(end_payload, 8, &out, &outlen);
    uint64_t doc_id = read_uint64(out + 1);
    free(out);
    
    // Verify all items are present
    Doc *doc_entry = find_doc(doc_id);
    ASSERT(doc_entry != NULL);
    
    lexbor_str_t str = {0};
    lxb_html_serialize_tree_str(lxb_dom_interface_node(doc_entry->doc), &str);
    
    // Check for first and last items
    ASSERT(memmem(str.data, str.length, "Item 1", 6) != NULL);
    ASSERT(memmem(str.data, str.length, "Item 50", 7) != NULL);
    
    lexbor_str_destroy(&str, doc_entry->doc->dom_document.text, false);
    remove_doc(doc_id);
}

TEST(stream_empty_chunk) {
    // Test that empty chunks are handled gracefully
    unsigned char *out = NULL;
    uint32_t outlen = 0;
    
    // Begin
    op_parse_stream_begin(&out, &outlen);
    uint64_t session_id = read_uint64(out + 1);
    free(out);
    
    // Send empty chunk
    unsigned char empty_payload[8];
    write_uint64(empty_payload, session_id);
    int result = op_parse_stream_chunk(empty_payload, 8, &out, &outlen);
    ASSERT(result == 1);
    ASSERT(out[0] == 0);  // should succeed
    free(out);
    
    // Send actual content
    const char *html = "<html><body><p>Test</p></body></html>";
    uint32_t html_len = strlen(html);
    unsigned char *html_payload = malloc(8 + html_len);
    write_uint64(html_payload, session_id);
    memcpy(html_payload + 8, html, html_len);
    op_parse_stream_chunk(html_payload, 8 + html_len, &out, &outlen);
    free(html_payload);
    free(out);
    
    // End
    unsigned char end_payload[8];
    write_uint64(end_payload, session_id);
    op_parse_stream_end(end_payload, 8, &out, &outlen);
    uint64_t doc_id = read_uint64(out + 1);
    free(out);
    
    remove_doc(doc_id);
}

/* ========================================================================
 * Test Runner
 * ======================================================================== */

int main(void) {
    printf("===========================================\n");
    printf("  Port Main Unit Tests\n");
    printf("===========================================\n\n");
    
    /* Tag comparison tests */
    RUN_TEST(tag_eq_exact_match);
    RUN_TEST(tag_eq_no_match);
    RUN_TEST(tag_eq_partial_match);
    RUN_TEST(tag_eq_empty);
    RUN_TEST(tag_eq_long_name);
    
    /* Buffer tests */
    RUN_TEST(sbuf_reserve_initial);
    RUN_TEST(sbuf_reserve_no_realloc_needed);
    RUN_TEST(sbuf_reserve_realloc_needed);
    RUN_TEST(sbuf_reserve_multiple_calls);
    RUN_TEST(sink_cb_basic);
    RUN_TEST(sink_cb_multiple_calls);
    
    /* Encoding/decoding tests */
    RUN_TEST(uint32_encoding);
    RUN_TEST(uint32_decoding);
    RUN_TEST(uint32_roundtrip);
    RUN_TEST(uint64_encoding);
    RUN_TEST(uint64_decoding);
    RUN_TEST(uint64_roundtrip);
    
    /* Document registry tests */
    RUN_TEST(doc_registry_add_single);
    RUN_TEST(doc_registry_add_multiple);
    RUN_TEST(doc_registry_find_existing);
    RUN_TEST(doc_registry_find_nonexistent);
    RUN_TEST(doc_registry_remove);
    RUN_TEST(doc_registry_remove_nonexistent);
    
    /* HTML parsing and serialization tests */
    RUN_TEST(html_parse_simple);
    RUN_TEST(html_parse_nested);
    RUN_TEST(html_parse_malformed);
    RUN_TEST(html_serialize_simple);
    
    /* DOM manipulation tests - attributes */
    RUN_TEST(dom_get_attribute);
    RUN_TEST(dom_set_attribute);
    RUN_TEST(dom_remove_attribute);
    
    /* DOM manipulation tests - text and HTML */
    RUN_TEST(dom_get_text);
    RUN_TEST(dom_set_text);
    RUN_TEST(dom_inner_html);
    
    /* DOM manipulation tests - node creation and tree manipulation */
    RUN_TEST(dom_create_element);
    RUN_TEST(dom_append_child);
    RUN_TEST(dom_insert_before);
    RUN_TEST(dom_remove_node);
    RUN_TEST(dom_complex_building);
    
    /* Streaming parser tests */
    RUN_TEST(stream_begin_end_basic);
    RUN_TEST(stream_multiple_chunks);
    RUN_TEST(stream_split_in_tag);
    RUN_TEST(stream_invalid_session_chunk);
    RUN_TEST(stream_invalid_session_end);
    RUN_TEST(stream_reuse_after_end);
    RUN_TEST(stream_large_chunks);
    RUN_TEST(stream_empty_chunk);
    
    /* Clean up document registry */
    if (docs) {
        for (size_t i = 0; i < docs_len; i++) {
            lxb_html_document_destroy(docs[i].doc);
        }
        free(docs);
    }
    
    printf("\n===========================================\n");
    printf("  Test Results\n");
    printf("===========================================\n");
    printf("Total tests:  %d\n", test_count);
    printf("Passed:       %d\n", test_passed);
    printf("Failed:       %d\n", test_failed);
    printf("===========================================\n");
    
    return (test_failed == 0) ? 0 : 1;
}
