#ifndef META_H_
#define META_H_

#include <cctype>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <stdarg.h>

#include <filesystem>

#include <clang-c/Index.h>
#include <clang-c/CXString.h>
#include <clang-c/CXFile.h>

#define XXH_INLINE_ALL
#include "xxHash/xxhash.h"

#define META_VERSION 1

// Core utilities
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define RMOV(...) static_cast<std::remove_reference_t<decltype(__VA_ARGS__)>&&>(__VA_ARGS__)
#define RFWD(...) static_cast<decltype(__VA_ARGS__)&&>(__VA_ARGS__)

template <typename F>
struct Defer {
    Defer(F f) : f(f) {}
    ~Defer() { f(); }
    F f;
};

template <typename F>
Defer<F> defer_create( F f ) {
    return Defer<F>( f );
}

#define defer__(line) defer_ ## line
#define defer_(line) defer__( line )

struct DeferDummy { };
template<typename F>
Defer<F> operator+ (DeferDummy, F&& f)
{
    return defer_create<F>(RFWD(f));
}

#define defer auto defer_( __LINE__ ) = DeferDummy( ) + [&]( )

// Diagnostics
#define BREAK()       asm("int $3")
#define DEBUG_BREAK() asm("int $3")

#ifndef DEBUG_LOG
#define DEBUG_LOG(...)\
    do {\
        if (debug_print_enabled>0) {\
            printf(__VA_ARGS__);\
            printf("\n");\
        }\
    } while(0)

#define DEBUG_LOGR(...)\
    do {\
        if (debug_print_enabled>0) {\
            printf(__VA_ARGS__);\
        }\
    } while(0)
#endif

#define PANIC(...)\
    do {\
        fprintf(stodut, "FATAL: " __VA_ARGS__);\
        fprintf(stderr, "\n");\
        BREAK();\
    } while(0)

#define FERROR(...)\
    do {\
        fprintf(stderr, "error: " __VA_ARGS__);\
        fprintf(stderr, "\n");\
        exit(1);\
    } while(0)

#define ERROR(cursor, ...)\
    do {\
        CXSourceLocation loc = clang_getCursorLocation(cursor);\
        CXFile file; unsigned line, column;\
        clang_getExpansionLocation(loc, &file, &line, &column, nullptr);\
        CXString filename = clang_getFileName(file);\
        fprintf(stderr, "%s:%d:%d: error: ", clang_getCString(filename), line, column);\
        fprintf(stderr, __VA_ARGS__);\
        fprintf(stderr, "\n");\
        trace_parent(cursor);\
        clang_disposeString(filename);\
        exit(1);\
    } while(0)

#define CPARSE_ERROR(cursor, msg)\
    ERROR(cursor, "[%s] error parsing commnet '%s': " msg, proc_sz, comment_sz)

#define PARSE_ERROR(stream,msg)\
    do {\
        CXSourceLocation loc = clang_getTokenLocation((stream)->tu, *(stream)->at);\
        CXFile file; unsigned line, column;\
        clang_getExpansionLocation(loc, &file, &line, &column, nullptr);\
        CXString filename = clang_getFileName(file);\
        fprintf(stderr, "%s:%d:%d: error: %s", clang_getCString(filename), line, column, msg);\
        clang_disposeString(filename);\
        exit(1);\
    } while (0)

// Platform compatibility
#if defined(_WIN32)
#define NOTHROW
#ifndef CRTIMP
#  if defined(_DLL) && !defined(_STATIC_CPPLIB)
#    define CRTIMP __declspec(dllimport)
#  else
#    define CRTIMP
#  endif
#endif

#undef strdup
#define strdup _strdup
#elif defined(__linux__)
#define NOTHROW __attribute__(( __nothrow__ __LEAF))
#define CRTIMP
#endif

extern "C" CRTIMP char* strerror(int errnum) NOTHROW;
extern "C" int strcmp(const char * str1, const char * str2) NOTHROW;
extern CRTIMP const char* strchr(const char * str1, int chr) NOTHROW;
extern const char* strchr(const char * str1, int chr) NOTHROW;
extern const char* strstr( const char* str, const char* substr ) NOTHROW;
extern const char* strrchr(const char * str1, int chr) NOTHROW;
extern "C" char* strdup(const char *str1 ) NOTHROW;
extern "C" void* memcpy(void *dst, const void *src, size_t size) NOTHROW;
extern "C" size_t strlen(const char * str) NOTHROW;

// Containers
template<typename T>
struct DynamicArray {
    T *data;
    int count, capacity;

    T* begin() { return &data[0]; }
    T* end()   { return &data[count]; }

    T& operator[](int i) { return data[i]; }
};

template<typename T>
int array_add(DynamicArray<T> *arr, T e)
{
    if (arr->count+1 > arr->capacity) {
        arr->capacity = MAX(arr->count+1, arr->capacity*2);
        arr->data = (T*)realloc(arr->data, arr->capacity*sizeof(T));
    }

    arr->data[arr->count] = e;
    return arr->count++;
}

template<typename T>
int array_find(DynamicArray<T> *arr, T e)
{
    for (int i = 0; i < arr->count; i++) {
        if (arr->data[i] == e) return i;
    }

    return -1;
}

template<>
int array_find(DynamicArray<char*> *arr, char *e)
{
    for (int i = 0; i < arr->count; i++) {
        if (strcmp(arr->data[i], e) == 0) return i;
    }

    return -1;
}


template<typename T>
struct ListIterator {
    T *ptr;

    operator T*() { return ptr; }
    operator T&() { return *ptr; }
    T* operator->() { return ptr; }

    bool operator!=(ListIterator<T> other) { return ptr != other.ptr; }

    ListIterator<T> operator*() { return *this; }
    ListIterator<T> operator++() { ptr = ptr->next; return *this; }

    template<typename E>
    bool operator==(E other) { return *ptr == other; }
};

template<typename T>
struct List {
    T head = {};
    T *ptr = &head;
    int count = 0;

    ListIterator<T> begin() { return { head.next }; }
    ListIterator<T> end() { return { nullptr }; }

    operator bool() { return head.next != nullptr; };
};

template<typename T>
T* list_push(List<T> *list, T *elem)
{
    list->ptr->next = elem;
    list->ptr = elem;
    list->count++;
    return elem;
}

template<typename T, typename... Args>
T* list_push(List<T> *list, Args... args)
{
    auto *decl = new T { args... };
    return list_push(list, decl);
}

template<typename T, typename E>
T* list_find(List<T> *list, E arg)
{
    for (auto *ptr = list->ptr; ptr; ptr = ptr->next) {
        if (*ptr == arg) return ptr;
    }

    if (list->ptr != &list->head) {
        for (auto ptr : *list) {
            if (ptr == arg) return ptr;
        }
    }

    return nullptr;
}

// Buffered output
struct StringBuilder {
    struct Buffer {
        struct Buffer *next;
        char data[4096];
        int count;
    } head;
    Buffer *current;
    int count;
};

struct HashedFile {
    StringBuilder stream;
    XXH3_state_t hash;
};

// Clang traversal
struct TokenStream {
    CXTranslationUnit tu;
    CXToken *at;
    CXToken *end;

    operator bool() { return at < end; }
};

struct CursorAttributes {
    int exported          : 1;
    int internal          : 1;
    int test              : 1;
    int integration_test  : 1;
};

struct ClangVisitorData {
    CXTranslationUnit tu;
    CursorAttributes attributes;

    struct {
        const char *h;
        const char *src;
    } in;

    const char *out_dir;

    struct {
        CXToken *tokens;
        unsigned token_count;
    } parent;
};

// Metadata declarations
struct Include {
    CXFile file;
    Include *next;
};

struct MetaArg {
    char *name;
    MetaArg *next;
};

struct MetaDecl {
    char *name;
    List<MetaArg> args;
    MetaDecl *next;
};

struct FieldDecl {
    CXType type;
    const char *name;
    bool is_base_type;
    List<MetaDecl> meta;
    FieldDecl *next;
};

struct FieldDeclVisitorData {
    List<FieldDecl> *fields;
};

struct ConstantDecl {
    char *name;
    long long value;
    ConstantDecl *next;
};

struct StructDecl {
    char *name;

    List<FieldDecl> fields;

    StructDecl *next;
    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct EnumDecl {
    char *name;

    CXType type;
    List<ConstantDecl> constants;

    EnumDecl *next;
    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct ProcDecl {
    char *name;

    CXCursor cursor;
    CursorAttributes attributes;

    ProcDecl *next;
};

// Debug state
extern const char *debug_trace_file;
extern const char *debug_trace_cursor;
extern int debug_print_enabled;

#endif // META_H_

#if defined(META_IMPL) && !defined(META_IMPL_ONCE_)
#define META_IMPL_ONCE_

// Debug state
const char *debug_trace_file = nullptr;
const char *debug_trace_cursor = nullptr;
int debug_print_enabled = 0;

// Containers
Include* list_find(List<Include> *list, CXFile arg)
{
    for (auto *ptr = list->ptr; ptr; ptr = ptr->next) {
        if (clang_File_isEqual(ptr->file, arg) != 0) return ptr;
    }

    if (list->ptr != &list->head) {
        for (auto ptr : *list) {
            if (clang_File_isEqual(ptr->file, arg) != 0) return ptr;
        }
    }

    return nullptr;
}

// Buffered output and hashing
void append_bytes(StringBuilder *sb, const void *data, int size)
{
    if (sb->current == nullptr) sb->current = &sb->head;

    int offset = 0;
    while (size) {
        int written = MIN(sizeof sb->current->data - sb->current->count, size);
        if (written > 0) {
            memcpy(sb->current->data+sb->current->count, ((char*)data)+offset, written);
            sb->current->count += written;
            size -= written;
            offset += written;
        }

        if (sb->current->count + size > sizeof sb->current->data) {
            sb->current->next = (StringBuilder::Buffer*)malloc(sizeof(StringBuilder::Buffer));
            memset(sb->current->next, 0, sizeof *sb->current->next);
            sb->current = sb->current->next;
        }
    }
}

void file_write_bytes(HashedFile *f, const void *data, int size)
{
    append_bytes(&f->stream, data, size);
    XXH3_128bits_update(&f->hash, data, size);
}

void file_write(HashedFile *f, const char *str)
{
    file_write_bytes(f, str, strlen(str));
}

void file_writec(HashedFile *f, char c)
{
    file_write_bytes(f, &c, 1);
}

void file_writef(HashedFile *f, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    char buffer[4096];

    int length = vsnprintf(buffer, sizeof buffer-1, fmt, args);
    va_end(args);

    if (length >= sizeof buffer) {
        FERROR("buffer overflow");
        return;
    }

    file_write_bytes(f, buffer, length);
}


XXH128_hash_t hash_file_on_disk(const char *path)
{
    XXH3_state_t state;
    XXH3_INITSTATE(&state);
    XXH3_128bits_reset_withSeed(&state, META_VERSION);

    if (FILE *fp = fopen(path, "rb"); fp) {
        char buf[4096];
        size_t n;
        while ((n = fread(buf, 1, sizeof buf, fp)) > 0) {
            XXH3_128bits_update(&state, buf, n);
        }
        fclose(fp);
    }

    return XXH3_128bits_digest(&state);
}

// Paths and text
const char* sz_extension_of(const char *path)
{
    char *ext = nullptr;
    for (const char *p = path; *p; p++) {
        if (*p == '.') ext = (char*)p;
        if (*p == '/' || *p == '\\') ext = nullptr;
    }

    return ext;
}

const char* sz_directory_of(const char *path)
{
    const char *last_sep = nullptr;
    for (const char *p = path; *p; p++) {
        if (*p == '/' || *p == '\\') last_sep = p;
    }

    if (!last_sep) return nullptr;

    char *dir = (char*)malloc((size_t)last_sep-(size_t)path+1);
    memcpy(dir, path, (size_t)last_sep-(size_t)path);
    dir[(size_t)last_sep-(size_t)path] = '\0';
    return dir;
}

bool is_alpha(char c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

bool is_numeric(char c)
{
    return c >= '0' && c <= '9';
}

bool is_newline(char c)
{
    return c == '\n' || c == '\r';
}

bool is_whitespace(char c)
{
    return c == ' ' || c == '\t' || is_newline(c);
}

bool eat_whitespace(const char **p, const char *end)
{
    if (**p != ' ' && **p != '\t' && **p != '\n' && **p != '\r') return true;

    do (*p)++;
    while (*p < end && (**p == ' ' || **p == '\t' || **p == '\n' || **p == '\r'));
    return true;
}

// Token streams
CXString clang_tokenString(TokenStream *stream)
{
    return clang_getTokenSpelling(stream->tu, stream->at[0]);
}

const char* token_kind_str(CXTokenKind kind)
{
    switch (kind) {
    case CXToken_Punctuation: return "Punctuation";
    case CXToken_Keyword: return "Keyword";
    case CXToken_Identifier: return "Identifier";
    case CXToken_Literal: return "Literal";
    case CXToken_Comment: return "Comment";
    }
    return "unknown";
}

bool require_next_token(TokenStream *stream, CXTokenKind kind)
{
    if (stream->at == stream->end) return false;
    stream->at++;

    return clang_getTokenKind(*stream->at) == kind;
}

bool next_token(TokenStream *stream, CXToken *out)
{
    if (stream->at == stream->end) return false;
    *out = stream->at[1];
    stream->at++;
    return true;
}

bool next_token(TokenStream *stream)
{
    if (stream->at == stream->end) return false;
    stream->at++;
    return true;
}

// Clang strings, paths, and types
bool clang_String_isNull(CXString string)
{
    const char *string_sz = clang_getCString(string);
    return !string_sz || string_sz[0] == '\0';
}

int clang_strcmp(CXString lhs, const char *rhs)
{
    if (clang_String_isNull(lhs)) return !rhs || rhs[0] == '\0' ? 0 : -1;
    if (!rhs || rhs[0] == '\0') return -1;

    return strcmp(clang_getCString(lhs), rhs);
}

int clang_strcmp(CXString lhs, CXString rhs)
{
    if (clang_String_isNull(lhs)) return clang_String_isNull(rhs) ? 0 : -1;
    if (clang_String_isNull(rhs)) return -1;
    return strcmp(clang_getCString(lhs), clang_getCString(rhs));
}

int clang_path_starts_with(CXString lhs, const char *rhs)
{
    if (clang_String_isNull(lhs)) return !rhs || rhs[0] == '\0' ? 0 : -1;
    if (!rhs || rhs[0] == '\0') return -1;

    const char *lhs_sz = clang_getCString(lhs);
    while (*lhs_sz && *rhs) {
        if (*lhs_sz != *rhs &&
            (*lhs_sz != '/' || *rhs != '\\') &&
            (*lhs_sz != '\\' || *rhs != '/'))
        {
            return -1;
        }

        lhs_sz++;
        rhs++;
    }

    return *rhs == '\0' ? 0 : -1;
}

int clang_str_starts_with(CXString lhs, const char *rhs)
{
    if (clang_String_isNull(lhs)) return !rhs || rhs[0] == '\0' ? 0 : -1;
    if (!rhs || rhs[0] == '\0') return -1;

    const char *lhs_sz = clang_getCString(lhs);
    while (*lhs_sz && *rhs) {
        if (*lhs_sz != *rhs) return -1;

        lhs_sz++;
        rhs++;
    }

    return *rhs == '\0' ? 0 : -1;
}

int clang_str_ends_with(CXString lhs, const char *rhs)
{
    if (clang_String_isNull(lhs)) return !rhs || rhs[0] == '\0' ? 0 : -1;
    if (!rhs || rhs[0] == '\0') return -1;

    const char *lhs_sz = clang_getCString(lhs);
    size_t lhs_len = strlen(lhs_sz), rhs_len = strlen(rhs);
    if (lhs_len < rhs_len) return -1;

    return strcmp(lhs_sz+lhs_len-rhs_len, rhs);
}

bool clang_isArray(CXType type)
{
    return
        type.kind == CXType_ConstantArray ||
        type.kind == CXType_IncompleteArray ||
        type.kind == CXType_VariableArray ||
        type.kind == CXType_DependentSizedArray;
}

// Clang cursors and source locations
bool clang_FunctionDecl_isDeclaredInline(CXTranslationUnit tu, CXCursor cursor)
{
    CXSourceRange range = clang_getCursorExtent(cursor);

    CXToken *tokens = nullptr;
    unsigned token_count = 0;
    clang_tokenize(tu, range, &tokens, &token_count);
    defer { clang_disposeTokens(tu, tokens, token_count); };

    for (unsigned i = 0; i < token_count; i++) {
        if (clang_getTokenKind(tokens[i]) != CXToken_Keyword) continue;

        CXString token_s = clang_getTokenSpelling(tu, tokens[i]);
        defer { clang_disposeString(token_s); };

        if (clang_strcmp(token_s, "inline") == 0) {
            return true;
        }
    }

    return false;
}

CXString clang_Cursor_getFilename(CXCursor cursor)
{
    CXSourceLocation location = clang_getCursorLocation(cursor);

    CXFile c_file{};
    clang_getFileLocation(location, &c_file, nullptr, nullptr, nullptr);
    return clang_getFileName(c_file);
}

bool clang_Cursor_isInFile(CXCursor cursor, const char *src, const char *header)
{
    CXString c_filename_s = clang_Cursor_getFilename(cursor);
    defer { clang_disposeString(c_filename_s); };

    const char *c_filename_sz = clang_getCString(c_filename_s);
    if (c_filename_sz) {

        for (auto *lhs = c_filename_sz, *rhs = src; *lhs && *rhs; lhs++, rhs++) {
            if (*lhs == *rhs) continue;
            if (*lhs == '/' && *rhs == '\\') continue;
            if (*lhs == '\\' && *rhs == '/') continue;
            goto not_in_src;
        }

        return true;
    not_in_src:;

        for (auto *lhs = c_filename_sz, *rhs = header; *lhs && *rhs; lhs++, rhs++) {
            if (*lhs == *rhs) continue;
            if (*lhs == '/' && *rhs == '\\') continue;
            if (*lhs == '\\' && *rhs == '/') continue;
            goto not_in_header;
        }

        return true;
    not_in_header:;
    }

    if (c_filename_sz && strcmp(c_filename_sz, src) == 0) return true;
    if (c_filename_sz && strcmp(c_filename_sz, header) == 0) return true;

    return false;
}

CXSourceRange clang_Cursor_getArgumentRange(
    CXCursor cursor,
    int arg_index)
{
    CXCursor arg_c = clang_Cursor_getArgument(cursor, arg_index);
    return clang_getCursorExtent(arg_c);
}

CXSourceLocation clang_Cursor_getArgumentRangeEnd(
    CXCursor cursor,
    int arg_index)
{
    CXSourceRange range = clang_Cursor_getArgumentRange(cursor, arg_index);
    return clang_getRangeEnd(range);
}

CXSourceLocation clang_Cursor_getArgumentRangeStart(
    CXCursor cursor,
    int arg_index)
{
    CXSourceRange range = clang_Cursor_getArgumentRange(cursor, arg_index);
    return clang_getRangeStart(range);
}

CXToken* clang_Cursor_getArgumentComment(
    CXTranslationUnit tu,
    CXCursor cursor,
    int arg_index,
    CXToken *tokens,
    unsigned token_count)
{
    CXSourceRange decl_range = clang_getCursorExtent(cursor);

    int arg_count = clang_Cursor_getNumArguments(cursor);

    CXSourceLocation arg_end = arg_index+1 < arg_count
        ? clang_Cursor_getArgumentRangeStart(cursor, arg_index+1)
        : clang_getRangeEnd(decl_range);

    CXSourceRange range_arg = clang_Cursor_getArgumentRange(cursor, arg_index);

    for (CXToken *it = tokens; it < tokens+token_count; it++) {
        CXSourceRange range_t = clang_getTokenExtent(tu, *it);
        if (range_t.end_int_data < range_arg.begin_int_data) continue;
        if (range_t.begin_int_data > arg_end.int_data) break;

        CXTokenKind kind_t = clang_getTokenKind(*it);
        if (kind_t == CXToken_Comment) return it;
    }

    return nullptr;
}

// Clang diagnostics
void trace_parent(CXCursor cursor)
{
    CXCursor parent = clang_getCursorSemanticParent(cursor);
    if (clang_Cursor_isNull(parent)) parent = clang_getCursorLexicalParent(cursor);
    if (clang_Cursor_isNull(parent)) return;

    CXString parent_s = clang_getCursorSpelling(parent);
    defer { clang_disposeString(parent_s); };

    CXCursorKind kind = clang_getCursorKind(parent);
    CXString kind_s = clang_getCursorKindSpelling(kind);
    defer { clang_disposeString(kind_s); };

    printf("\tparent: %s (%s)", clang_getCString(parent_s), clang_getCString(kind_s));
}

void clang_printRange(CXSourceRange range)
{
    CXSourceLocation begin = clang_getRangeStart(range);
    CXSourceLocation end = clang_getRangeEnd(range);

    unsigned line0, column0;
    unsigned line1, column1;
    clang_getFileLocation(begin, nullptr, &line0, &column0, nullptr);

    clang_getFileLocation(end, nullptr, &line1, &column1, nullptr);

    DEBUG_LOG("range: [%u:%u, %u:%u]", line0, column0,
                line1, column1);
}

CXChildVisitResult clang_printChildren(
    CXCursor cursor,
    CXCursor /*parent*/,
    CXClientData /*client_data*/)
{
    CXString cursor_s = clang_getCursorSpelling(cursor);
    defer { clang_disposeString(cursor_s); };

    auto cursor_kind = clang_getCursorKind(cursor);
    CXString cursor_kind_s = clang_getCursorKindSpelling(cursor_kind);
    defer { clang_disposeString(cursor_kind_s); };

    DEBUG_LOG(
        "%s; %s",
        clang_getCString(cursor_s),
        clang_getCString(cursor_kind_s));

    return CXChildVisit_Recurse;
}

CXChildVisitResult clang_debugDumpChildren(
    CXCursor cursor,
    CXCursor parent,
    CXClientData client_data)
{
    int saved_debug_print_enabled = debug_print_enabled;
    defer { debug_print_enabled = saved_debug_print_enabled; };

    ClangVisitorData *data = (ClangVisitorData*)client_data;

    CXString parent_s = clang_getCursorSpelling(parent);
    CXString cursor_s = clang_getCursorSpelling(cursor);
    defer {
        clang_disposeString(cursor_s);
        clang_disposeString(parent_s);
    };

    if (debug_trace_cursor && clang_strcmp(cursor_s, debug_trace_cursor) == 0) {
        debug_print_enabled++;
    }

    CXCursorKind kind_c = clang_getCursorKind(cursor);
    CXString kind_c_s = clang_getCursorKindSpelling(kind_c);
    defer { clang_disposeString(kind_c_s); };


    DEBUG_LOGR("[%s] ", clang_getCString(parent_s));
    DEBUG_LOGR("%s", clang_getCString(kind_c_s));

    if (!clang_String_isNull(cursor_s))
        DEBUG_LOGR(" : %s", clang_getCString(cursor_s));

    CXSourceRange range_c = clang_getCursorExtent(cursor);
    CXSourceLocation begin = clang_getRangeStart(range_c);
    CXSourceLocation end = clang_getRangeEnd(range_c);

    unsigned line0, column0;
    unsigned line1, column1;
    clang_getFileLocation(begin, nullptr, &line0, &column0, nullptr);

    clang_getFileLocation(end, nullptr, &line1, &column1, nullptr);
    DEBUG_LOGR(" : [%u:%u, %u:%u]", line0, column0, line1, column1);
    DEBUG_LOGR("\n");

    if (kind_c == CXCursor_ParmDecl ||
        kind_c == CXCursor_FunctionDecl)
    {
        CXToken *tokens = nullptr; unsigned token_count = 0;
        clang_tokenize(data->tu, range_c, &tokens, &token_count);
        defer { clang_disposeTokens(data->tu, tokens, token_count); };

        for (CXToken *it = tokens; it < tokens+token_count; it++) {
            CXTokenKind kind_t = clang_getTokenKind(*it);
            const char *kind_sz = token_kind_str(kind_t);

            CXString token_s = clang_getTokenSpelling(data->tu, *it);
            defer { clang_disposeString(token_s); };

            DEBUG_LOG("\t[%s] : '%s'", kind_sz, clang_getCString(token_s));
        }

    }

    clang_visitChildren(cursor, clang_debugDumpChildren, client_data);
    return CXChildVisit_Continue;
}

#endif // defined(META_IMPL_) && !defined(META_IMPL_ONCE_)
