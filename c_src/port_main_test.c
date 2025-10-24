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
