#include <cctype>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <filesystem>

#include <clang-c/Index.h>
#include <clang-c/CXString.h>


#if defined(_WIN32)
#define NOTHROW
#define CRTIMP __declspec(dllimport)

#undef strdup
#define strdup _strdup
#elif defined(__linux__)
#define NOTHROW __attribute__(( __nothrow__ __LEAF))
#define CRTIMP
#endif


extern "C" CRTIMP char* strerror(int errnum) NOTHROW;
extern "C" int strcmp(const char * str1, const char * str2) NOTHROW;
extern "C" CRTIMP char* strdup(const char *str1 ) NOTHROW;
extern "C" void* memcpy(void *dst, const void *src, size_t size) NOTHROW;
extern "C" size_t strlen(const char * str) NOTHROW;

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


#define BREAK()       asm("int $3")
#define DEBUG_BREAK() asm("int $3")

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

struct {
    bool generate_flecs = false;
} opts;

const char *debug_trace_file = nullptr;
const char *debug_trace_cursor = nullptr;
int debug_print_enabled = 0;

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

#define NUM_COMPONENT_ARGS 1
#define NUM_ENUM_ARGS 1
#define NUM_TAG_ARGS 1

struct TokenStream {
    CXTranslationUnit tu;
    CXToken *at;
    CXToken *end;

    operator bool() { return at < end; }
};

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

bool eat_whitespace(const char **p, const char *end)
{
    if (**p != ' ' && **p != '\t' && **p != '\n' && **p != '\r') return true;

    do (*p)++;
    while (*p < end && (**p == ' ' || **p == '\t' || **p == '\n' || **p == '\r'));
    return true;
}

struct CursorAttributes {
    int exported  : 1;
    int internal  : 1;
    int in_namespace : 1;
};

struct ClangVisitorData {
    CXTranslationUnit tu;
    CursorAttributes attributes;

    struct {
        const char *h;
        const char *src;
    } in;

    struct {
        CXToken *tokens;
        unsigned token_count;
    } parent;
};


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
    T head;
    T *ptr = &head;

    ListIterator<T> begin() { return { head.next }; }
    ListIterator<T> end() { return { nullptr }; }

    operator bool() { return head.next != nullptr; };
};

template<typename T>
void list_push(List<T> *list, T *elem)
{
    T *ptr = list->ptr;
    while (ptr->next) ptr = ptr->next;

    ptr->next = elem;
    list->ptr = elem;
}

template<typename T, typename... Args>
T* list_push(List<T> *list, Args... args)
{
    auto *decl = new T { args... };
    list_push(list, decl);
    return decl;
}

template<typename T, typename E>
T* list_find(List<T> *list, E arg)
{
    for (T *ptr = list->ptr; ptr; ptr = ptr->next) {
        if (*ptr == arg) return ptr;
    }

    if (list->ptr != &list->head) {
        for (auto ptr : *list) {
            if (ptr == arg) return ptr;
        }
    }

    return nullptr;
}

struct FieldDecl {
    CXType type;
    const char *name;
    bool is_base_type;
    FieldDecl *next;
};

struct ConstantDecl {
    const char *name;
    long long value;
    ConstantDecl *next;
};

struct StructDecl {
    const char *name;

    List<FieldDecl> fields;

    StructDecl *next;
    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct EnumDecl {
    const char *name;
    CXType type;
    List<ConstantDecl> constants;

    EnumDecl *next;
    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct ProcDecl {
    CXCursor cursor;
    CursorAttributes attributes;

    ProcDecl *next;
};

struct ComponentArg {
    const char *name;

    ComponentArg *next;

    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct ComponentDecl {
    const char *name;
    List<ComponentArg> args[NUM_COMPONENT_ARGS];

    ComponentDecl *next;

    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct TagDecl {
    const char *name;
    List<ComponentArg> args[NUM_TAG_ARGS];

    TagDecl *next;

    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct EnumTagDecl{
    const char *name;
    List<ComponentArg> args[NUM_ENUM_ARGS];

    EnumTagDecl *next;

    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

// TODO(jesper): at least the struct decls really ought to be a hashmap at this point because it'll contain every structure in the translation unit, regardless of whether or not we need the type info, because we can't really back-track if we determine we need it
List<StructDecl> struct_decls{};
List<EnumDecl> enum_decls{};
List<ProcDecl> internal_proc_decls{};
List<ProcDecl> public_proc_decls{};

List<ComponentDecl> component_decls{};
List<TagDecl> tag_decls{};
List<EnumTagDecl> enum_tag_decls{};

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

CXChildVisitResult clang_getAttributes(
    CXCursor cursor,
    CXCursor /*parent*/,
    CXClientData client_data)
{
    CursorAttributes *data = (CursorAttributes*)client_data;

    auto cursor_kind = clang_getCursorKind(cursor);
    if (clang_isAttribute(cursor_kind)) {
        CXString cursor_name = clang_getCursorSpelling(cursor);
        defer { clang_disposeString(cursor_name); };

        if (clang_strcmp(cursor_name, "export") == 0) {
            data->exported = true;
        } else if (clang_strcmp(cursor_name, "internal") == 0) {
            data->exported = true;
            data->internal = true;
        } else {
            DEBUG_LOG("unknown annotation: %s",  clang_getCString(cursor_name));
        }
    }

    return CXChildVisit_Continue;
}

CXChildVisitResult clang_pushFieldDecls(
    CXCursor cursor,
    CXCursor /*parent*/,
    CXClientData client_data)
{
    List<FieldDecl> *dst = (List<FieldDecl>*)client_data;

    auto cursor_kind = clang_getCursorKind(cursor);
    if (cursor_kind == CXCursor_FieldDecl) {
        CXString field_s = clang_getCursorSpelling(cursor);
        defer { clang_disposeString(field_s); };

        CXType type = clang_getCursorType(cursor);
        list_push(dst, type, strdup(clang_getCString(field_s)));
    } else if (cursor_kind == CXCursor_UnionDecl) {
        clang_visitChildren(
            cursor,
            [](CXCursor cursor, CXCursor /*parent*/, CXClientData client_data) -> CXChildVisitResult
            {
                List<FieldDecl> *dst = (List<FieldDecl>*)client_data;

                auto cursor_kind = clang_getCursorKind(cursor);
                if (cursor_kind == CXCursor_FieldDecl) {
                    CXString field_s = clang_getCursorSpelling(cursor);
                    defer { clang_disposeString(field_s); };

                    CXType type = clang_getCursorType(cursor);
                    list_push(dst, type, strdup(clang_getCString(field_s)));
                    return CXChildVisit_Break;
                } else if (cursor_kind == CXCursor_StructDecl) {
                    clang_visitChildren(cursor, clang_pushFieldDecls, client_data);
                    return CXChildVisit_Break;
                } else {
                    return CXChildVisit_Continue;
                }
            },
            client_data);
    } else if (cursor_kind == CXCursor_CXXBaseSpecifier) {
        CXType type = clang_getCursorType(cursor);
        CXString type_s = clang_getTypeSpelling(type);
        defer { clang_disposeString(type_s); };

        list_push(dst, type, strdup(clang_getCString(type_s)), true);

        auto *struct_decl = list_find(&struct_decls, clang_getCString(type_s));
        if (struct_decl == nullptr) {
            ERROR(cursor, "unknown base type: %s", clang_getCString(type_s));
            return CXChildVisit_Break;
        }
    }

    return CXChildVisit_Continue;
}

CXChildVisitResult clang_pushConstantDecls(
    CXCursor cursor,
    CXCursor /*parent*/,
    CXClientData client_data)
{
    List<ConstantDecl> *dst = (List<ConstantDecl>*)client_data;

    auto cursor_kind = clang_getCursorKind(cursor);
    if (cursor_kind == CXCursor_EnumConstantDecl) {
        CXString name = clang_getCursorSpelling(cursor);
        defer { clang_disposeString(name); };
        list_push(dst, strdup(clang_getCString(name)), clang_getEnumConstantDeclValue(cursor));
    }

    return CXChildVisit_Continue;
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
    if (c_filename_sz && strcmp(c_filename_sz, src) == 0) return true;
    if (c_filename_sz && strcmp(c_filename_sz, header) == 0) return true;

    DEBUG_LOG("skipping cursor in file: %s, not in src (%s) or its header (%s)", c_filename_sz, src, header);
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

template<typename T>
bool parse_decl_macro(List<T> *decls, CXTranslationUnit tu, CXCursor cursor, int arg_max)
{
    CXSourceRange range = clang_getCursorExtent(cursor);

    CXToken *tokens = nullptr; unsigned token_count = 0;
    clang_tokenize(tu, range, &tokens, &token_count);
    defer { clang_disposeTokens(tu, tokens, token_count); };

    TokenStream stream{ tu, tokens, tokens+token_count };

    int paren = 1;
    if (!require_next_token(&stream, CXToken_Punctuation)) {
        PARSE_ERROR(&stream, "expected punctuation");
        return false;
    }

    if (clang_strcmp(clang_tokenString(&stream), "(") != 0) {
        PARSE_ERROR(&stream, "expected open paren");
        return false;
    }

    if (!require_next_token(&stream, CXToken_Identifier)) {
        PARSE_ERROR(&stream, "expected identifier");
        return false;
    }

    CXString decl_s = clang_getTokenSpelling(stream.tu, *stream.at);
    defer { clang_disposeString(decl_s); };
    list_push(decls, strdup(clang_getCString(decl_s)));

    CXToken t;
    int arg_index = -1;
    while (paren > 0) {
        if (!next_token(&stream, &t)) exit(1);
        CXTokenKind kind = clang_getTokenKind(t);

        if (kind == CXToken_Punctuation) {
            CXString tok_s = clang_getTokenSpelling(stream.tu, t);
            defer { clang_disposeString(tok_s); };

            if (clang_strcmp(tok_s, "(") == 0) paren++;
            else if (clang_strcmp(tok_s, ")") == 0) paren--;
            else if (clang_strcmp(tok_s, ",") == 0) {
                arg_index++;
                if (arg_index > arg_max) {
                    ERROR(cursor, "too many arguments for decl: %s\n", decls->ptr->name);
                    return false;
                }
            } else if (clang_strcmp(tok_s, "|") == 0) {
                if (arg_index < 0) {
                    ERROR(cursor, "unexpected argument separator for decl: %s\n", decls->ptr->name);
                    return false;
                }
            } else {
                ERROR(cursor, "unexpected puncutation: %s\n", clang_getCString(tok_s));
                return false;
            }
        } else if (kind == CXToken_Identifier) {
            if (arg_index < 0) ERROR(cursor, "unexpected argument\n");
            CXString tok_s = clang_getTokenSpelling(stream.tu, t);
            defer { clang_disposeString(tok_s); };
            list_push(&decls->ptr->args[arg_index], strdup(clang_getCString(tok_s)));
        }
    }

    return true;
}

void write_proc_decl(FILE *f, CXTranslationUnit tu, CXCursor cursor, CursorAttributes attributes)
{
    CXString cursor_s = clang_getCursorSpelling(cursor);
    defer { clang_disposeString(cursor_s); };


    CXString proc_s = cursor_s;
    const char *proc_sz = clang_getCString(proc_s);
    DEBUG_LOG("generating proc decl: %s", proc_sz);

    defer { fwrite("\n", 1, 1, f); };
    if (attributes.in_namespace) {
        if (!attributes.internal) fprintf(f, "namespace PUBLIC { ");
        else fprintf(f, "namespace { ");
    }
    defer { if (attributes.in_namespace) fprintf(f, " }"); };

    CX_StorageClass storage = clang_Cursor_getStorageClass(cursor);
    switch (storage) {
    case CX_SC_Invalid:
        break;
    case CX_SC_None:
        if (!attributes.in_namespace) {
            if (!attributes.internal) fwrite("extern ", 1, 7, f);
            else fwrite("static ", 1, 7, f);
        } break;
    case CX_SC_Extern:
        fwrite("extern ", 1, 7, f);
        break;
    case CX_SC_Static:
        fwrite("static ", 1, 7, f);
        break;
    case CX_SC_PrivateExtern:
    case CX_SC_OpenCLWorkGroupLocal:
    case CX_SC_Auto:
    case CX_SC_Register:
        break;
    }

    CXType t = clang_getCursorType(cursor);
    CXType ret_t = clang_getResultType(t);

    CXString ret_t_s = clang_getTypeSpelling(ret_t);
    defer { clang_disposeString(ret_t_s); };
    DEBUG_LOG("\tret type: %s", clang_getCString(ret_t_s));

    fprintf(f, "%s", clang_getCString(ret_t_s));
    if (ret_t.kind != CXType_Pointer) fwrite(" ", 1, 1, f);
    fprintf(f, "%s(", proc_sz);
    defer { fwrite(");", 1, 2, f); };

    int arg_count = clang_Cursor_getNumArguments(cursor);
    if (arg_count) DEBUG_LOG("\targ count: %d", arg_count);

    if (arg_count) {
        CXSourceRange range = clang_getCursorExtent(cursor);

        CXToken *tokens = nullptr; unsigned token_count = 0;
        clang_tokenize(tu, range, &tokens, &token_count);
        defer { clang_disposeTokens(tu, tokens, token_count); };

        if (!token_count) {
            ERROR(cursor, "no tokens for decl: '%s', move 'EXPORT' decl to end of declarationt to work-around",
                  proc_sz);
        }

        int paren = 0;
        for (CXToken *it = tokens; it < tokens+token_count; it++) {
            CXTokenKind kind = clang_getTokenKind(*it);
            if (kind == CXToken_Punctuation) {
                CXString token_s = clang_getTokenSpelling(tu, *it);
                defer { clang_disposeString(token_s); };

                if (clang_strcmp(token_s, "(") == 0) paren++;
                else if (clang_strcmp(token_s, ")") == 0 && --paren == 0) {
                    token_count = it-tokens;
                    break;
                }
            }
        }

        for (int i = 0; i < arg_count; i++) {
            CXCursor arg_c = clang_Cursor_getArgument(cursor, i);
            CXType arg_t = clang_getCursorType(arg_c);

            CXString arg_s = clang_getCursorSpelling(arg_c);
            defer { clang_disposeString(arg_s); };

            CXToken *arg_comment = clang_Cursor_getArgumentComment(
                tu,
                cursor, i,
                tokens, token_count);

            bool has_arg_name = clang_getCString(arg_s)[0] != '\0';

            if (!has_arg_name) {
                DEBUG_LOG("\tno arg name");

                CXSourceRange range = clang_getCursorExtent(cursor);
                CXSourceLocation begin = clang_getRangeStart(range);
                CXSourceLocation end = clang_getRangeEnd(range);

                unsigned begin_line, begin_column;
                clang_getFileLocation(begin, nullptr, &begin_line, &begin_column, nullptr);

                unsigned end_line, end_column;
                clang_getFileLocation(end, nullptr, &end_line, &end_column, nullptr);

                DEBUG_LOG("\targument range [%u:%u, %u:%u]", begin_line, begin_column,
                          end_line, end_column);
            }

            if (arg_t.kind == CXType_Invalid) {
                ERROR(arg_c, "invalid type for argument '%s'\n", clang_getCString(arg_s));
            }

            if (clang_isArray(arg_t)) {
                CXType elem_t = clang_getElementType(arg_t);
                long long elem_count = clang_getNumElements(arg_t);

                CXString arg_t_s = clang_getTypeSpelling(elem_t);
                defer { clang_disposeString(arg_t_s); };

                fprintf(f, "%s", clang_getCString(arg_t_s));

                if (has_arg_name) fwrite(" ", 1, 1, f);
                fprintf(f, "%s[%lld]", clang_getCString(arg_s), elem_count);
            } else {
                CXString arg_t_s = clang_getTypeSpelling(arg_t);

                defer { clang_disposeString(arg_t_s); };
                fprintf(f, "%s", clang_getCString(arg_t_s));

                if (arg_t.kind != CXType_Pointer && has_arg_name) fwrite(" ", 1, 1, f);
                fprintf(f, "%s", clang_getCString(arg_s));
            }

            if (arg_comment) {
                CXString comment_s = clang_getTokenSpelling(
                    tu, *arg_comment);
                defer { clang_disposeString(comment_s); };

                const char *comment_sz = clang_getCString(comment_s);

                DEBUG_LOG(
                    "\targ[%d] comment: '%s'",
                    i, comment_sz);

                const char *p = comment_sz;
                const char *end = comment_sz+strlen(comment_sz);


                if (*p++ != '/') CPARSE_ERROR(arg_c, "expected '/'");
                if (*p++ != '*') CPARSE_ERROR(arg_c, "expected '*'");
                if (!eat_whitespace(&p, end)) CPARSE_ERROR(arg_c, "end of stream");

                if (*p == '=') {
                    const char *default_v = p;

                    do p++;
                    while (p < end-1 && !(p[0] == '*' && p[1] == '/'));
                    while (p > default_v && p[-1] == ' ') p--;

                    int length = (int)(p-default_v);
                    fprintf(f, " %.*s", length, default_v);
                    DEBUG_LOG("\targ[%d] default value: '%.*s'", i, length, default_v);
                }
            }

            if (i < arg_count - 1) fwrite(", ", 1, 2, f);
        }
    }
}

CXChildVisitResult clang_visitor(
    CXCursor cursor,
    CXCursor parent,
    CXClientData client_data)
{
    ClangVisitorData *parent_d = (ClangVisitorData*)client_data;
    ClangVisitorData cursor_d = *parent_d;

    CXTranslationUnit tu = cursor_d.tu;
    auto *in = &cursor_d.in;

    int saved_debug_print_enabled = debug_print_enabled;
    defer { debug_print_enabled = saved_debug_print_enabled; };

    CXString cursor_s = clang_getCursorSpelling(cursor);
    defer { clang_disposeString(cursor_s); };

    if (debug_trace_file) {
        CXString filename = clang_Cursor_getFilename(cursor);
        defer { clang_disposeString(filename); };
        if (clang_str_ends_with(filename, debug_trace_file) == 0) {
            debug_print_enabled=1;
        }
    }

    if (debug_trace_cursor && clang_strcmp(cursor_s, debug_trace_cursor) == 0)
        debug_print_enabled=1;

    CXString c_filename = clang_Cursor_getFilename(cursor);
    CXString p_filename = clang_Cursor_getFilename(parent);
    defer { clang_disposeString(c_filename); clang_disposeString(p_filename); };

    if (clang_Cursor_hasAttrs(cursor))
        clang_visitChildren(cursor, clang_getAttributes, &cursor_d.attributes);

    auto cursor_kind = clang_getCursorKind(cursor);
    if (cursor_kind == CXCursor_Namespace) {
        if (!cursor_d.attributes.exported) return CXChildVisit_Continue;
        cursor_d.attributes.in_namespace = true;
        clang_visitChildren(cursor, clang_visitor, &cursor_d);
    } else if (cursor_kind == CXCursor_FunctionDecl) {
        if (!cursor_d.attributes.exported) return CXChildVisit_Continue;

        if (!clang_Cursor_isInFile(cursor, in->src, in->h)) {
            return CXChildVisit_Continue;
        }

        if (cursor_d.attributes.internal) {
            list_push(&internal_proc_decls, cursor, cursor_d.attributes);
        } else {
            list_push(&public_proc_decls, cursor, cursor_d.attributes);
        }
    } else if (cursor_kind == CXCursor_StructDecl) {
        CXType type = clang_getCursorType(cursor);
        CXString type_s = clang_getTypeSpelling(type);
        defer { clang_disposeString(type_s); };

        auto *decl = list_push(&struct_decls, strdup(clang_getCString(type_s)));
        clang_visitChildren(cursor, clang_pushFieldDecls, &decl->fields);
    } else if (cursor_kind == CXCursor_EnumDecl) {
        CXType type = clang_getCursorType(cursor);
        CXString type_s = clang_getTypeSpelling(type);
        defer { clang_disposeString(type_s); };

        CXType underlying = clang_getEnumDeclIntegerType(cursor);
        auto *decl = list_push(&enum_decls, strdup(clang_getCString(type_s)), underlying);
        clang_visitChildren(cursor, clang_pushConstantDecls, &decl->constants);
    } else if (cursor_kind == CXCursor_MacroExpansion) {
        if (!clang_Cursor_isInFile(cursor, in->src, in->h)) {
            return CXChildVisit_Continue;
        }

        CXString macro_s = clang_getCursorSpelling(cursor);
        defer { clang_disposeString(macro_s); };
        const char *macro_sz = clang_getCString(macro_s);

        if (opts.generate_flecs) {
            if (strcmp(macro_sz, "ECS_COMPONENT") == 0) {
                if (!parse_decl_macro(&component_decls, tu, cursor, NUM_COMPONENT_ARGS))
                    ERROR(cursor, "error parsing component decl");
            } else if (strcmp(macro_sz, "ECS_ENUM") == 0) {
                if (!parse_decl_macro(&enum_tag_decls, tu, cursor, NUM_ENUM_ARGS))
                    ERROR(cursor, "error parsing enum decl");
            } else if (strcmp(macro_sz, "ECS_TAG") == 0) {
                if (!parse_decl_macro(&tag_decls, tu, cursor, NUM_TAG_ARGS))
                    ERROR(cursor, "error parsing tag decl");
            }
        }
    }

    return CXChildVisit_Continue;
}

void emit_include_guard_begin(FILE *f, const char *prefix, const char *name, const char *suffix)
{
    char guard[4096];
    if (prefix && suffix) snprintf(guard, sizeof guard, "%s_%s_%s", prefix, name, suffix);
    else if (prefix) snprintf(guard, sizeof guard, "%s_%s", prefix, name);
    else if (suffix) snprintf(guard, sizeof guard, "%s_%s", name, suffix);
    else snprintf(guard, sizeof guard, "%s", name);

    fprintf(f, "#ifndef %s_H\n", guard);
    fprintf(f, "#define %s_H\n\n", guard);
}

void emit_include_guard_end(FILE *f, const char *prefix, const char *name, const char *suffix)
{
    char guard[4096];
    if (prefix && suffix) snprintf(guard, sizeof guard, "%s_%s_%s", prefix, name, suffix);
    else if (prefix) snprintf(guard, sizeof guard, "%s_%s", prefix, name);
    else if (suffix) snprintf(guard, sizeof guard, "%s_%s", name, suffix);
    else snprintf(guard, sizeof guard, "%s", name);

    fprintf(f, "\n#endif // %s_H\n", guard);
}

template<typename T>
void emit_decls(FILE *f, List<T> decls, const char *fmt)
{
    if (!decls) return;

    fprintf(f, "\n");
    for (auto decl : decls) {
        fprintf(f, fmt, decl->name);
        fprintf(f, "\n");
    }
}

void emit_include(FILE *f, const char *path)
{
    fprintf(f, "#include \"%s\"\n", path);
}

void emit_flecs_component_members(FILE *f, StructDecl *decl)
{
    for (auto field : decl->fields) {
        if (field->is_base_type) {
            auto *base_decl = list_find(&struct_decls, field->name);
            if (!base_decl) FERROR("no base decl for field: %s", field->name);

            fprintf(f, "\n\t\t.member(Ecs%s, 0, \"%s\", 1, offsetof(%s, %s))",
                    field->name, field->name,
                    decl->name, base_decl->fields.head.next->name);
        } else if (clang_isArray(field->type)) {
            CXType elem_t = clang_getElementType(field->type);
            long long elem_count = clang_getNumElements(field->type);

            CXString field_t_s = clang_getTypeSpelling(elem_t);
            defer { clang_disposeString(field_t_s); };

            fprintf(f, "\n\t\t.member<%s>(\"%s\", %lld, offsetof(%s, %s))",
                    clang_getCString(field_t_s),
                    field->name,
                    elem_count,
                    decl->name, field->name);
        } else {
            CXString field_t_s = clang_getTypeSpelling(field->type);
            defer { clang_disposeString(field_t_s); };

            fprintf(f, "\n\t\t.member(\"%s\", &%s::%s)",
                    field->name,
                    decl->name, field->name);
        }
    }
}

bool generate_header(const char *out_path, const char *src_path, CXTranslationUnit tu)
{
    CXCursor cursor = clang_getTranslationUnitCursor(tu);

    const char *src_ext = src_path+strlen(src_path)-1;
    while (src_ext > src_path && src_ext[-1] != '.') src_ext--;

    const char *src_filename = src_ext;
    while (src_filename > src_path &&
           src_filename[-1] != '/' &&
           src_filename[-1] != '\\')
    {
        src_filename--;
    }

    int src_name_len = int(strlen(src_filename) - strlen(src_ext)) - 1;

    char *h_path = strdup(src_path);
    char *p = h_path + strlen(h_path);
    while (p > h_path && p[-1] != '.') p--;
    *p++ = 'h'; *p = '\0';

    ClangVisitorData data{
        .tu     = tu,
        .in.h   = h_path,
        .in.src = src_path,
    };

    char internal_out_path[4096];
    snprintf(internal_out_path, sizeof internal_out_path, "%s/internal", out_path);

    char flecs_out_path[4096];
    snprintf(flecs_out_path, sizeof flecs_out_path, "%s/flecs", out_path);

    std::filesystem::create_directories(out_path);
    std::filesystem::create_directories(internal_out_path);
    std::filesystem::create_directories(flecs_out_path);

    char name[4096];
    snprintf(name, sizeof name, "%.*s", src_name_len, src_filename);
    for (char *p = name; *p; p++) *p = toupper(*p);


    clang_visitChildren(cursor, clang_visitor, &data);

    //if (public_proc_decls)
    {
        FILE *f = nullptr;

        char path[4096];
        snprintf(path, sizeof path, "%s/%.*s.h", out_path, src_name_len, src_filename);
        if (f = fopen(path, "wb"); !f) {
            FERROR("failed to open out.file '%s': %s\n", path, strerror(errno));
        }
        defer { fclose(f); };

        emit_include_guard_begin(f, nullptr, name, "PUBLIC");
        defer { emit_include_guard_end(f, nullptr, name, "PUBLIC"); };

        fprintf(f, "namespace PUBLIC {}\n");
        fprintf(f, "using namespace PUBLIC;\n\n");

        for (auto decl : public_proc_decls) {
            write_proc_decl(f, tu, decl->cursor, decl->attributes);
        }
    }

    //if (internal_proc_decls)
    {
        FILE *f = nullptr;

        char path[4096];
        snprintf(path, sizeof path, "%s/%.*s.h", internal_out_path, src_name_len, src_filename);
        if (f = fopen(path, "wb"); !f) {
            FERROR("failed to open out.file '%s': %s\n", path, strerror(errno));
        }
        defer { fclose(f); };

        emit_include_guard_begin(f, nullptr, name, "INTERNAL");
        defer { emit_include_guard_end(f, nullptr, name, "INTERNAL"); };

        for (auto decl : internal_proc_decls) {
            write_proc_decl(f , tu, decl->cursor, decl->attributes);
        }
    }

    if (opts.generate_flecs) {
        FILE *f = nullptr;

        char path[4096];
        snprintf(path, sizeof path, "%s/%.*s.h", flecs_out_path, src_name_len, src_filename);
        if (f = fopen(path, "wb"); !f) {
            FERROR("failed to open out.file '%s': %s\n", path, strerror(errno));
        }
        defer { fclose(f); };

        if (component_decls || tag_decls || enum_tag_decls) {
            emit_include_guard_begin(f, "FLECS", name, nullptr);
            emit_include(f, "flecs_util.h");

            fprintf(f, "\nextern void flecs_register_%.*s(flecs::world &ecs);\n", src_name_len, src_filename);

            emit_decls(f, tag_decls, "extern ECS_TAG_DECLARE(%s);");
            emit_decls(f, enum_tag_decls, "extern ECS_COMPONENT_DECLARE(%s);");
            emit_decls(f, component_decls, "extern ECS_COMPONENT_DECLARE(%s);");

            if (component_decls) {
                fprintf(f, "\n");
                for (auto decl : enum_tag_decls) {
                    fprintf(f, "#define Ecs%s ecs_id(%s)\n", decl->name, decl->name);
                }
                for (auto decl : component_decls) {
                    fprintf(f, "#define Ecs%s ecs_id(%s)\n", decl->name, decl->name);
                }
            }
            emit_include_guard_end(f, "FLECS", name, nullptr);

            fprintf(f, "\n\n#if defined(FLECS_%s_IMPL)\n", name);
            emit_include_guard_begin(f, "FLECS", name, "IMPL_ONCE");

            emit_decls(f, tag_decls, "ECS_TAG_DECLARE(%s);");
            emit_decls(f, enum_tag_decls, "ECS_COMPONENT_DECLARE(%s);");
            emit_decls(f, component_decls, "ECS_COMPONENT_DECLARE(%s);");

            fprintf(f, "\nvoid flecs_register_%.*s(flecs::world &ecs)\n{\n", src_name_len, src_filename);
            for (auto decl : tag_decls) {
                fprintf(f, "\tECS_TAG_DEFINE(ecs, %s);\n", decl->name);
                fprintf(f, "\tecs_add_id(ecs, ecs_id(%s), EcsTag);\n", decl->name);

                for (auto arg : decl->args[0]) {
                    fprintf(f, "\tecs_add_id(ecs, ecs_id(%s), %s);\n", decl->name, arg->name);
                }
                if (decl->next) fprintf(f, "\n");
            }

            if (enum_tag_decls && tag_decls) fprintf(f, "\n");

            for (auto decl : enum_tag_decls) {
                fprintf(f, "\tECS_COMPONENT_DEFINE(ecs, %s);\n", decl->name);
                fprintf(f, "\tecs_add(ecs, Ecs%s, EcsEnum);\n", decl->name);

                for (auto arg : decl->args[0]) {
                    fprintf(f, "\tecs_add_id(ecs, Ecs%s, %s);\n", decl->name, arg->name);
                }

                auto *enum_decl = list_find(&enum_decls, decl->name);
                if (!enum_decl) ERROR(cursor, "no enum decl for tag: %s", decl->name);

                for (auto constant : enum_decl->constants) {
                    fprintf(f, "\t{\tecs_entity_desc_t desc = { .name = \"%s\" };\n", constant->name);
                    fprintf(f, "\t\tecs_entity_t c = ecs_entity_init(ecs, &desc);\n");
                    fprintf(f, "\t\tecs_add(ecs, c, EcsEnum);\n");
                    fprintf(f, "\t\tecs_i32_t v = %lld;\n", constant->value);
                    fprintf(f, "\t\tecs_set_id(ecs, c, ecs_pair(EcsConstant, ecs_id(ecs_i32_t)), sizeof v, &v);\n");
                    fprintf(f, "\t}\n");
                }
            }

            if (component_decls && (enum_tag_decls || tag_decls)) fprintf(f, "\n");

            for (auto decl : component_decls) {
                fprintf(f, "\tECS_COMPONENT_DEFINE(ecs, %s);\n", decl->name);
                fprintf(f, "\tecs.component<%s>()", decl->name);

                if (auto *struct_decl = list_find(&struct_decls, decl->name);
                    struct_decl)
                {
                    emit_flecs_component_members(f, struct_decl);
                } else {
                    ERROR(cursor, "no struct or enum decl for component: %s", decl->name);
                }

                for (auto arg : decl->args[0]) fprintf(f, "\n\t\t.add(%s)", arg->name);

                fprintf(f, ";\n");
                if (decl->next) fprintf(f, "\n");
            }
            fprintf(f, "}\n");

            emit_include_guard_end(f, "FLECS", name, "IMPL_ONCE");
            fprintf(f, "#endif // defined(FLECS_%s_IMPL)\n", name);
        }
    }

    return true;
}

int print_usage()
{
    printf("usage: meta -o <out.path> <src.file> [--flecs]\n");
    return 1;
}

int main(int argc, char **argv)
{
    const char *out_path = "./";
    const char *src_filename  = nullptr;

    int fargc = 0;
    char **fargv = nullptr;

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            if (argv[i][1] == '-' && strcmp(&argv[i][2], "flecs") == 0) {
                opts.generate_flecs = true;
            } else if (argv[i][1] == 'o') {
                out_path = argv[++i];
            } else if (argv[i][1] == '-' && argv[i][2] == '\0') {
                fargc = argc - i - 1;
                fargv = argv + i + 1;
                break;
            } else {
                printf("unhandled argv[%d]:%s\n", i, argv[i]);
            }
        } else {
            src_filename = argv[i];
        }
    }

    if (!src_filename) {
        FERROR("no source file specified\n");
        return print_usage();
    }

    if (!out_path) {
        FERROR("no out.path specified\n");
        return print_usage();
    }

    out_path = sz_directory_of(out_path);

    CXIndex index = clang_createIndex(0, 0);

    unsigned int flags =
        //CXTranslationUnit_SkipFunctionBodies |
        CXTranslationUnit_DetailedPreprocessingRecord |
        0;

    CXTranslationUnit tu;
    CXErrorCode result = clang_parseTranslationUnit2(
        index,
        src_filename,
        fargv, fargc,
        nullptr, 0,
        flags,
        &tu);
    defer { clang_disposeTranslationUnit(tu); };

    if (result) {
        const char *result_s = "";
        switch (result) {
        case CXError_Success: break;
        case CXError_Failure: result_s = "Failure"; break;
        case CXError_Crashed: result_s = "Crashed"; break;
        case CXError_InvalidArguments: result_s = "InvalidArguments"; break;
        case CXError_ASTReadError: result_s = "ASTReadError"; break;
        }

        FERROR("failed to parse translation unit: '%s'\n", result_s);
    }

    if (!generate_header(out_path, src_filename, tu)) return 1;
    return 0;
}
