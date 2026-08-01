#define META_IMPL
#include "meta.h"

struct {
    const char *depfile = nullptr;
} opts;

struct ComponentArg {
    char *name;
    char *second;
    ComponentArg *next;

    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct ComponentDecl {
    char *name;
    List<ComponentArg> args;

    ComponentDecl *next;

    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct TagDecl {
    char *name;
    List<ComponentArg> args;

    TagDecl *next;

    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct EnumTagDecl{
    char *name;
    List<ComponentArg> args;

    EnumTagDecl *next;

    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

struct ModuleDecl {
    char *name;
    List<ComponentArg> args;

    ModuleDecl *next;

    bool operator==(const char *rhs) { return name && rhs && strcmp(name, rhs) == 0; }
};

List<Include> includes;

// TODO(jesper): at least the struct decls really ought to be a hashmap at this point because it'll contain every structure in the translation unit, regardless of whether or not we need the type info, because we can't really back-track if we determine we need it
List<StructDecl> struct_decls{};
List<EnumDecl> enum_decls{};
List<ProcDecl> internal_proc_decls{};
List<ProcDecl> public_proc_decls{};

List<ProcDecl> test_proc_decls{};
List<ProcDecl> integration_test_proc_decls{};

List<ComponentDecl> flecs_component_decls{};
List<TagDecl> flecs_tag_decls{};
List<EnumTagDecl> flecs_enum_tag_decls{};
List<ModuleDecl> flecs_module_decls{};

struct FlecsIntegerType {
    const char *name;
    bool is_unsigned;
};

FlecsIntegerType flecs_integer_type(CXType type)
{
    bool is_unsigned;
    switch (type.kind) {
    case CXType_Char_U:
    case CXType_UChar:
    case CXType_UShort:
    case CXType_UInt:
    case CXType_ULong:
    case CXType_ULongLong:
        is_unsigned = true;
        break;
    case CXType_Char_S:
    case CXType_SChar:
    case CXType_Short:
    case CXType_Int:
    case CXType_Long:
    case CXType_LongLong:
        is_unsigned = false;
        break;
    default:
        return {};
    }

    switch (clang_Type_getSizeOf(type)) {
    case 1: return { is_unsigned ? "ecs_u8_t"  : "ecs_i8_t",  is_unsigned };
    case 2: return { is_unsigned ? "ecs_u16_t" : "ecs_i16_t", is_unsigned };
    case 4: return { is_unsigned ? "ecs_u32_t" : "ecs_i32_t", is_unsigned };
    case 8: return { is_unsigned ? "ecs_u64_t" : "ecs_i64_t", is_unsigned };
    default: return {};
    }
}

bool parse_meta_attr(List<MetaDecl> *dst, const char *text)
{
    const char *p = text;
    if (strncmp(p, "meta(", 5) != 0) return false;
    p += 5;

    while (*p && *p != ')') {
        while (is_whitespace(*p) || *p == ',') p++;
        if (!(is_alpha(*p) || *p == '_')) return false;

        const char *name = p;
        while (is_alpha(*p) || is_numeric(*p) || *p == '_') p++;
        size_t name_len = (size_t)(p-name);
        char *name_sz = (char*)malloc(name_len+1);
        if (!name_sz) FERROR("out of memory");
        memcpy(name_sz, name, name_len);
        name_sz[name_len] = '\0';

        MetaDecl *meta = list_push(dst, name_sz);

        while (is_whitespace(*p)) p++;

        if (*p == '{' && *p++) {
            while (*p && *p != '}') {
                while (is_whitespace(*p) || *p == ',') p++;
                if (*p == '}') break;

                const char *arg = p;
                while (is_alpha(*p) || is_numeric(*p) || *p == '_') p++;
                if (p == arg) return false;

                size_t arg_len = (size_t)(p-arg);
                char *arg_sz = (char*)malloc(arg_len+1);
                if (!arg_sz) FERROR("out of memory");
                memcpy(arg_sz, arg, arg_len);
                arg_sz[arg_len] = '\0';

                list_push(&meta->args, arg_sz);
            }

            if (*p++ != '}') return false;
        }

        while (is_whitespace(*p)) p++;
        if (*p == ',') p++;
    }

    return *p == ')';
}

CXChildVisitResult clang_pushFieldMetaAttrs(
    CXCursor cursor,
    CXCursor /*parent*/,
    CXClientData client_data)
{
    FieldDecl *field = (FieldDecl*)client_data;

    auto cursor_kind = clang_getCursorKind(cursor);
    if (cursor_kind == CXCursor_AnnotateAttr) {
        CXString cursor_s = clang_getCursorSpelling(cursor);
        defer { clang_disposeString(cursor_s); };

        if (strncmp(clang_getCString(cursor_s), "meta(", 5) != 0) return CXChildVisit_Continue;

        if (!parse_meta_attr(&field->meta, clang_getCString(cursor_s))) {
            ERROR(cursor, "error parsing META attribute");
        }
    }

    return CXChildVisit_Continue;
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
        } else if (clang_strcmp(cursor_name, "test") == 0) {
            data->exported = true;
            data->test = true;
        } else if (clang_strcmp(cursor_name, "integration_test") == 0) {
            data->exported = true;
            data->integration_test = true;
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
    FieldDeclVisitorData *data = (FieldDeclVisitorData*)client_data;

    auto cursor_kind = clang_getCursorKind(cursor);
    if (cursor_kind == CXCursor_FieldDecl) {
        CXString field_s = clang_getCursorSpelling(cursor);
        defer { clang_disposeString(field_s); };

        CXType type = clang_getCursorType(cursor);
        FieldDecl *field = list_push(data->fields, type, strdup(clang_getCString(field_s)));
        clang_visitChildren(cursor, clang_pushFieldMetaAttrs, field);
    } else if (cursor_kind == CXCursor_UnionDecl) {
        clang_visitChildren(
            cursor,
            [](CXCursor cursor, CXCursor /*parent*/, CXClientData client_data) -> CXChildVisitResult
            {
                FieldDeclVisitorData *data = (FieldDeclVisitorData*)client_data;

                auto cursor_kind = clang_getCursorKind(cursor);
                if (cursor_kind == CXCursor_FieldDecl) {
                    CXString field_s = clang_getCursorSpelling(cursor);
                    defer { clang_disposeString(field_s); };

                    CXType type = clang_getCursorType(cursor);
                    FieldDecl *field = list_push(data->fields, type, strdup(clang_getCString(field_s)));
                    clang_visitChildren(cursor, clang_pushFieldMetaAttrs, field);
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

        list_push(data->fields, type, strdup(clang_getCString(type_s)), true);

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

template<typename T>
bool parse_decl_macro(List<T> *decls, CXTranslationUnit tu, CXCursor cursor)
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

    T *decl = list_push(decls, strdup(clang_getCString(decl_s)));

    CXToken t;
    bool is_pair = false;
    while (paren > 0) {
        if (!next_token(&stream, &t)) exit(1);
        CXTokenKind kind = clang_getTokenKind(t);

        if (kind == CXToken_Punctuation) {
            CXString tok_s = clang_getTokenSpelling(stream.tu, t);
            defer { clang_disposeString(tok_s); };

            if (clang_strcmp(tok_s, "(") == 0) paren++;
            else if (clang_strcmp(tok_s, ")") == 0) paren--;
            else if (clang_strcmp(tok_s, ",") == 0) {
            } else if (clang_strcmp(tok_s, "|") == 0) {
                is_pair = true;
            } else {
                ERROR(cursor, "unexpected puncutation: %s\n", clang_getCString(tok_s));
                return false;
            }
        } else if (kind == CXToken_Identifier) {
            CXString tok_s = clang_getTokenSpelling(stream.tu, t);
            defer { clang_disposeString(tok_s); };
            if (is_pair) decl->args.ptr->second = strdup(clang_getCString(tok_s));
            else list_push(&decl->args, strdup(clang_getCString(tok_s)));
            is_pair = false;
        }
    }

    return true;
}

void emit_proc_decl(HashedFile *f, CXTranslationUnit tu, CXCursor cursor, CursorAttributes attributes)
{
    CXString cursor_s = clang_getCursorSpelling(cursor);
    defer { clang_disposeString(cursor_s); };


    CXString proc_s = cursor_s;
    const char *proc_sz = clang_getCString(proc_s);
    DEBUG_LOG("generating proc decl: %s", proc_sz);

    defer { file_write(f, "\n"); };

    CX_StorageClass storage = clang_Cursor_getStorageClass(cursor);
    switch (storage) {
    case CX_SC_Invalid:
        break;
    case CX_SC_None:
        if (!attributes.internal) file_write(f, "extern ");
        else file_write(f, "static ");
        break;
    case CX_SC_Extern:
        file_write(f, "extern ");
        break;
    case CX_SC_Static:
        file_write(f, "static ");
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

    file_write(f, clang_getCString(ret_t_s));
    if (ret_t.kind != CXType_Pointer) file_write(f, " ");
    file_writef(f, "%s(", proc_sz);
    defer { file_write(f, ");"); };

    int arg_count = clang_Cursor_getNumArguments(cursor);
    if (arg_count) DEBUG_LOG("\targ count: %d", arg_count);

    if (arg_count) {
        CXSourceRange range = clang_getCursorExtent(cursor);

        CXToken *tokens = nullptr; unsigned token_count = 0;
        clang_tokenize(tu, range, &tokens, &token_count);
        defer { clang_disposeTokens(tu, tokens, token_count); };

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

                file_write(f, clang_getCString(arg_t_s));

                if (has_arg_name) file_write(f, " ");
                file_writef(f, "%s[%lld]", clang_getCString(arg_s), elem_count);
            } else {
                CXString arg_t_s = clang_getTypeSpelling(arg_t);

                defer { clang_disposeString(arg_t_s); };
                file_write(f, clang_getCString(arg_t_s));

                if (arg_t.kind != CXType_Pointer && has_arg_name)
                    file_write(f, " ");
                file_write(f, clang_getCString(arg_s));
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
                    file_writef(f, " %.*s", length, default_v);
                    DEBUG_LOG("\targ[%d] default value: '%.*s'", i, length, default_v);
                }
            }

            if (i < arg_count - 1) file_write(f, ", ");
        }
    }

    if (clang_Cursor_isVariadic(cursor)) {
        if (arg_count) file_write(f, ", ");
        file_write(f, "..."); 
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

    CXString c_filename = clang_Cursor_getFilename(cursor);
    CXString p_filename = clang_Cursor_getFilename(parent);
    defer { clang_disposeString(c_filename); clang_disposeString(p_filename); };

    CXString cursor_s = clang_getCursorSpelling(cursor);
    const char *cursor_sz = clang_getCString(cursor_s);
    defer { clang_disposeString(cursor_s); };

    int tracing_file = 0;
    if (debug_trace_file) {
        if (clang_str_ends_with(c_filename, debug_trace_file) == 0) {
            debug_print_enabled++;
            tracing_file = 1;
        }
    }

    int tracing_cursor = 0;
    if (debug_trace_cursor &&
        clang_strcmp(cursor_s, debug_trace_cursor) == 0)
    {
        debug_print_enabled++;
        tracing_cursor++;
    }

    defer {
        if (tracing_file) debug_print_enabled--;
        if (tracing_cursor) debug_print_enabled--;
    };


    if (clang_path_starts_with(c_filename, cursor_d.out_dir) == 0) {
        DEBUG_LOG("skipping cursor in output dir: %s", clang_getCString(c_filename));
        return CXChildVisit_Continue;
    }

    if (clang_strcmp(c_filename, p_filename) != 0) {
        DEBUG_LOG("cursor [%s] in file: %s", cursor_sz, clang_getCString(c_filename));
    }

    auto cursor_kind = clang_getCursorKind(cursor);
    if (cursor_kind == CXCursor_FunctionDecl) {
        CXLinkageKind linkage = clang_getCursorLinkage(cursor);
        bool inlined = clang_FunctionDecl_isDeclaredInline(tu, cursor);
        cursor_d.attributes.internal = linkage == CXLinkage_Internal;
        cursor_d.attributes.exported = !inlined && (linkage == CXLinkage_Internal || linkage == CXLinkage_External);
    }

    if (clang_Cursor_hasAttrs(cursor))
        clang_visitChildren(cursor, clang_getAttributes, &cursor_d.attributes);

    if (cursor_kind == CXCursor_FunctionDecl) {
        if (!cursor_d.attributes.exported) return CXChildVisit_Continue;

        if (!clang_Cursor_isInFile(cursor, in->src, in->h)) {
            DEBUG_LOG("skipping cursor: '%s', not in src (%s) or its header (%s)", cursor_sz, in->src, in->h);
            return CXChildVisit_Continue;
        }

        const char *proc_decl_sz = cursor_sz;
        if (cursor_d.attributes.internal) {
            DEBUG_LOG("internal proc: %s", proc_decl_sz);
            list_push(&internal_proc_decls, strdup(proc_decl_sz), cursor, cursor_d.attributes);
        } else if (cursor_d.attributes.test) {
            DEBUG_LOG("test proc: %s", proc_decl_sz);
            list_push(&test_proc_decls, strdup(proc_decl_sz), cursor, cursor_d.attributes);
        } else if (cursor_d.attributes.integration_test) {
            DEBUG_LOG("integration test proc: %s", proc_decl_sz);
            list_push(&integration_test_proc_decls, strdup(proc_decl_sz), cursor, cursor_d.attributes);
        } else {
            DEBUG_LOG("public proc: %s", proc_decl_sz);
            list_push(&public_proc_decls, strdup(proc_decl_sz), cursor, cursor_d.attributes);
        }
    } else if (cursor_kind == CXCursor_StructDecl) {
        CXType type = clang_getCursorType(cursor);
        CXString type_s = clang_getTypeSpelling(type);
        defer { clang_disposeString(type_s); };

        auto *decl = list_push(&struct_decls, strdup(clang_getCString(type_s)));
        FieldDeclVisitorData field_data{ .fields = &decl->fields };
        clang_visitChildren(cursor, clang_pushFieldDecls, &field_data);
    } else if (cursor_kind == CXCursor_EnumDecl) {
        if (!clang_isCursorDefinition(cursor)) return CXChildVisit_Continue;

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

        if (strcmp(macro_sz, "ECS_COMPONENT") == 0) {
            if (!parse_decl_macro(&flecs_component_decls, tu, cursor))
                ERROR(cursor, "error parsing component decl");
        } else if (strcmp(macro_sz, "ECS_ENUM") == 0) {
            if (!parse_decl_macro(&flecs_enum_tag_decls, tu, cursor))
                ERROR(cursor, "error parsing enum decl");
        } else if (strcmp(macro_sz, "ECS_TAG") == 0) {
            if (!parse_decl_macro(&flecs_tag_decls, tu, cursor))
                ERROR(cursor, "error parsing tag decl");
        } else if (strcmp(macro_sz, "ECS_MODULE_DECLARE") == 0) {
            if (!parse_decl_macro(&flecs_module_decls, tu, cursor))
                ERROR(cursor, "error parsing module decl");
        }
    } else if (cursor_kind == CXCursor_InclusionDirective) {
        CXFile file = clang_getIncludedFile(cursor);
        CXString file_s = clang_File_tryGetRealPathName(file);
        defer { clang_disposeString(file_s); };

        const char *file_sz = clang_getCString(file_s);
        if (!file_sz) return CXChildVisit_Continue;

        if (strstr(file_sz, "/usr/") ||
            strstr(file_sz, "/lib/"))
        {
            return CXChildVisit_Continue;
        }

        CXSourceRange range = clang_getCursorExtent(cursor);
        CXTranslationUnit tu = clang_Cursor_getTranslationUnit(cursor);

        CXToken* tokens;
        unsigned num_tokens;
        clang_tokenize(tu, range, &tokens, &num_tokens);
        defer { clang_disposeTokens(tu, tokens, num_tokens); };

        for (unsigned i = 0; i < num_tokens; i++) {
            CXString token_str = clang_getTokenSpelling(tu, tokens[i]);
            defer { clang_disposeString(token_str); };
            const char* token_cstr = clang_getCString(token_str);

            if (token_cstr[0] == '<') {
                return CXChildVisit_Continue;
            } else if (token_cstr[0] == '"') {
                break;
            }
        }

        if (clang_path_starts_with(file_s, cursor_d.out_dir) != 0 &&
            !list_find(&includes, file))
        {
            list_push(&includes, file);
        }
    }

    return CXChildVisit_Continue;
}

void emit_include_guard_begin(HashedFile *f, const char *prefix, const char *name, const char *suffix)
{
    char guard[4096];
    if (prefix && suffix) snprintf(guard, sizeof guard, "%s_%s_%s", prefix, name, suffix);
    else if (prefix) snprintf(guard, sizeof guard, "%s_%s", prefix, name);
    else if (suffix) snprintf(guard, sizeof guard, "%s_%s", name, suffix);
    else snprintf(guard, sizeof guard, "%s", name);

    file_writef(f, "#ifndef %s_H\n", guard);
    file_writef(f, "#define %s_H\n\n", guard);
}

void emit_include_guard_end(HashedFile *f, const char *prefix, const char *name, const char *suffix)
{
    char guard[4096];
    if (prefix && suffix) snprintf(guard, sizeof guard, "%s_%s_%s", prefix, name, suffix);
    else if (prefix) snprintf(guard, sizeof guard, "%s_%s", prefix, name);
    else if (suffix) snprintf(guard, sizeof guard, "%s_%s", name, suffix);
    else snprintf(guard, sizeof guard, "%s", name);

    file_writef(f, "\n#endif // %s_H\n", guard);
}

template<typename T>
void emit_decls(HashedFile *f, List<T> decls, const char *fmt)
{
    if (!decls) return;

    for (auto decl : decls) {
        file_writef(f, fmt, decl->name);
        file_write(f, "\n");
    }
}

template<typename T>
void emit_decls_ln(HashedFile *f, List<T> decls, const char *fmt)
{
    if (!decls) return;
    file_write(f, "\n");
    emit_decls(f, decls, fmt);
}

void emit_include(HashedFile *f, const char *path)
{
    file_writef(f, "#include \"%s\"\n", path);
}

void emit_flecs_component_members(HashedFile *f, StructDecl *decl)
{
    for (auto field : decl->fields) {
        if (field->is_base_type) {
            auto *base_decl = list_find(&struct_decls, field->name);
            if (!base_decl) FERROR("no base decl for field: %s", field->name);

            file_writef(f, "\n\t\t.member(Ecs%s, 0, \"%s\", 0, offsetof(%s, %s))",
                    field->name, field->name,
                    decl->name, base_decl->fields.head.next->name);
        } else if (clang_isArray(field->type)) {
            CXType elem_t = clang_getElementType(field->type);
            long long elem_count = clang_getNumElements(field->type);

            CXString field_t_s = clang_getTypeSpelling(elem_t);
            defer { clang_disposeString(field_t_s); };

            file_writef(f, "\n\t\t.member<%s>(\"%s\", %lld, offsetof(%s, %s))",
                    clang_getCString(field_t_s),
                    field->name,
                    elem_count,
                    decl->name, field->name);
        } else {
            CXString field_t_s = clang_getTypeSpelling(field->type);
            defer { clang_disposeString(field_t_s); };

            if (!strcmp(clang_getCString(field_t_s), "ecs_entity_t")) {
                file_writef(f, "\n\t\t.member(flecs::Entity, \"%s\", 0, offsetof(%s, %s))",
                        field->name,
                        decl->name, field->name);
            } else {
                file_writef(f, "\n\t\t.member(\"%s\", &%s::%s)",
                        field->name,
                        decl->name, field->name);
            }
        }
    }
}

bool has_flecs_meta(FieldDecl *field)
{
    for (auto meta : field->meta) {
        if (strcmp(meta->name, "EcsRequiredId") == 0) return true;
        if (strcmp(meta->name, "ColorRgb") == 0) return true;
    }

    return false;
}

bool has_flecs_meta(StructDecl *decl)
{
    for (auto field : decl->fields) {
        if (has_flecs_meta(field)) return true;
    }

    return false;
}

bool has_flecs_meta()
{
    for (auto decl : flecs_component_decls) {
        if (auto *struct_decl = list_find(&struct_decls, decl->name);
            struct_decl && has_flecs_meta(struct_decl))
        {
            return true;
        }
    }

    return false;
}

void emit_ecs_name(HashedFile *f, const char *name)
{
    if (name[0] == 'E' && name[1] == 'c' && name[2] == 's') file_writef(f, "%s", name);
    else file_writef(f, "Ecs%s", name);
}

void emit_flecs_meta(HashedFile *f, StructDecl *decl)
{
    for (auto field : decl->fields) {
        if (!has_flecs_meta(field)) continue;

        CXString field_t_s = clang_getTypeSpelling(field->type);
        defer { clang_disposeString(field_t_s); };

        file_writef(f, "\t{\n");
        file_writef(f, "\t\tecs_entity_t member = ecs_lookup_child(ecs, Ecs%s, \"%s\");\n", decl->name, field->name);
        file_writef(f, "\t\tif (!member) member = ecs_entity(ecs, {\n");
        file_writef(f, "\t\t\t.name = \"%s\",\n", field->name);
        file_writef(f, "\t\t\t.parent = Ecs%s,\n", decl->name);
        file_writef(f, "\t\t});\n");

        for (auto meta : field->meta) {
            if (strcmp(meta->name, "EcsRequiredId") == 0) {
                for (auto arg : meta->args) {
                    file_writef(f, "\t\tecs_add_pair(ecs, member, EcsRequiredId, ");
                    emit_ecs_name(f, arg->name);
                    file_writef(f, ");\n");
                }
            } else if (strcmp(meta->name, "ColorRgb") == 0) {
                file_writef(f, "\t\tecs_add_id(ecs, member, ");
                emit_ecs_name(f, meta->name);
                file_writef(f, ");\n");
            }
        }

        file_writef(f, "\t}\n");
    }
}

void emit_flecs_add_id(HashedFile *f, const char *entity, ComponentArg *arg)
{
    if (arg->second) file_writef(f, "\tecs_add_id(ecs, %s, ecs_pair(%s, %s));\n", entity, arg->name, arg->second);
    else file_writef(f, "\tecs_add_id(ecs, %s, %s);\n", entity, arg->name);
}

bool has_flecs_decl_meta()
{
    return flecs_tag_decls || flecs_enum_tag_decls || flecs_component_decls;
}

bool has_flecs_component_decl_meta()
{
    for (auto decl : flecs_component_decls) {
        if (decl->args) return true;
    }

    return false;
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
        .tu      = tu,
        .in.h    = h_path,
        .in.src  = src_path,
        .out_dir = out_path,
    };

    char name[4096];
    snprintf(name, sizeof name, "%.*s", src_name_len, src_filename);
    for (char *p = name; *p; p++) *p = toupper(*p);

    clang_visitChildren(cursor, clang_visitor, &data);

    bool generate_tests = test_proc_decls;
    bool generate_integration_tests = integration_test_proc_decls;
    bool generate_flecs = flecs_component_decls || flecs_tag_decls || flecs_enum_tag_decls;
    bool generate_flecs_meta = has_flecs_decl_meta() || has_flecs_meta();

    if (flecs_component_decls && !flecs_module_decls) {
        ERROR(cursor, "flecs components require ECS_MODULE_DECLARE");
    }

    std::filesystem::create_directories(out_path);

    //if (public_proc_decls || internal_proc_decls)
    {
        char path[4096];
        snprintf(path, sizeof path, "%s/%.*s.h", out_path, src_name_len, src_filename);

        HashedFile f{};
        XXH3_INITSTATE(&f.hash);
        XXH3_128bits_reset_withSeed(&f.hash, META_VERSION);

        defer {
            XXH128_hash_t hash = XXH3_128bits_digest(&f.hash);
            XXH128_hash_t curr = hash_file_on_disk(path);

            if (!XXH128_isEqual(hash, curr)) {
                if (FILE *fp = fopen(path, "wb")) {
                    for (auto *it = &f.stream.head; it; it = it->next) {
                        fwrite(it->data, 1, it->count, fp);
                    }
                    fclose(fp);
                } else {
                    FERROR("failed to open file '%s': %s\n", path, strerror(errno));
                }
            }
        };

        if (public_proc_decls || generate_flecs) {
            for (auto decl : flecs_enum_tag_decls) {
                auto *enum_decl = list_find(&enum_decls, decl->name);
                if (!enum_decl) ERROR(cursor, "no enum decl for tag: %s", decl->name);
                if (enum_decl->constants.count == 0) ERROR(cursor, "enum tag has no constants: %s", decl->name);
                if (!flecs_integer_type(enum_decl->type).name) ERROR(cursor, "unsupported enum underlying type: %s", decl->name);
            }


            emit_include_guard_begin(&f, nullptr, name, "GENERATED");

            for (auto decl : public_proc_decls) {
                emit_proc_decl(&f, tu, decl->cursor, decl->attributes);
            }

            if (generate_flecs) {
                file_writef(&f, "\nextern void flecs_register_%.*s(flecs::world &ecs);\n", src_name_len, src_filename);

                if (generate_flecs_meta) {
                    file_writef(&f, "extern void flecs_register_%.*s_meta(flecs::world &ecs);\n", src_name_len, src_filename);
                }

                emit_decls_ln(&f, flecs_tag_decls, "extern ECS_COMPONENT_DECLARE(%s);");
                emit_decls_ln(&f, flecs_component_decls, "extern ECS_COMPONENT_DECLARE(%s);");

                for (auto decl : flecs_enum_tag_decls) {
                    file_writef(&f, "\n#define Ecs%s ecs_id(%s)\n", decl->name, decl->name);
                    file_writef(&f, "extern ECS_COMPONENT_DECLARE(%s);\n", decl->name);
                    auto *enum_decl = list_find(&enum_decls, decl->name);
                    emit_decls(&f, enum_decl->constants, "extern ECS_COMPONENT_DECLARE(%s);");
                }

                if (flecs_component_decls || flecs_tag_decls) {
                    file_write(&f, "\n");
                    for (auto decl : flecs_tag_decls) {
                        file_writef(&f, "#define Ecs%s ecs_id(%s)\n", decl->name, decl->name);
                    }
                    for (auto decl : flecs_component_decls) {
                        file_writef(&f, "#define Ecs%s ecs_id(%s)\n", decl->name, decl->name);
                    }
                }
            }

            emit_include_guard_end(&f, nullptr, name, "GENERATED");
        }

        file_writef(&f, "\n#ifdef %s_GENERATED_IMPL\n", name);
        file_writef(&f, "#define %s_INTERNAL\n", name);
        file_write(&f, "#endif\n");

        if (internal_proc_decls) {
            file_writef(&f,
                "\n#if defined(%s_INTERNAL) && !defined(%s_INTERNAL_ONCE)\n",
                name, name);
            file_writef(&f, "#define %s_INTERNAL_ONCE\n\n", name);
            defer { file_write(&f, "\n#endif\n"); };

            for (auto decl : internal_proc_decls) {
                emit_proc_decl(&f , tu, decl->cursor, decl->attributes);
            }
        }

        if (generate_flecs) {
            file_writef(&f, "\n#if defined(%s_GENERATED_IMPL) && !defined(%s_GENERATED_IMPL_ONCE)\n", name, name);
            file_writef(&f, "#define %s_GENERATED_IMPL_ONCE\n", name);

            emit_decls_ln(&f, flecs_tag_decls,       "ECS_COMPONENT_DECLARE(%s);");
            emit_decls_ln(&f, flecs_component_decls, "ECS_COMPONENT_DECLARE(%s);");

            for (auto decl : flecs_enum_tag_decls) {
                file_writef(&f, "\nECS_COMPONENT_DECLARE(%s);\n", decl->name);
                auto *enum_decl = list_find(&enum_decls, decl->name);
                emit_decls(&f, enum_decl->constants, "ECS_COMPONENT_DECLARE(%s);");
            }

            file_writef(&f, "\nvoid flecs_register_%.*s(flecs::world &ecs)\n{\n", src_name_len, src_filename);
            if (flecs_module_decls) {
                file_writef(&f, "\textern ECS_COMPONENT_DECLARE(%s);\n", flecs_module_decls.head.next->name);
                file_writef(&f, "\tecs_entity_t prev_scope = ecs_set_scope(ecs, ecs_id(%s));\n\n", flecs_module_decls.head.next->name);
            }

            for (auto decl : flecs_tag_decls) {
                file_writef(&f, "\tECS_TAG_DEFINE(ecs, %s);\n", decl->name);
            }

            if (flecs_tag_decls) {
                file_write(&f, "\n#ifdef __cplusplus__\n");
                for (auto decl : flecs_tag_decls) {
                    file_writef(&f, "\tecs.component<%s>(nullptr, true, ecs_id(%s));\n", decl->name, decl->name);
                }
                file_write(&f, "#endif // __cplusplus__\n");
            }

            if (flecs_enum_tag_decls && flecs_tag_decls) file_write(&f, "\n");

            for (auto decl : flecs_enum_tag_decls) {
                file_writef(&f, "\tECS_COMPONENT_DEFINE(ecs, %s);\n", decl->name);
                auto *enum_decl = list_find(&enum_decls, decl->name);
                FlecsIntegerType underlying = flecs_integer_type(enum_decl->type);
                file_writef(&f, "\t{\n\t\tecs_enum_desc_t desc = {\n\t\t\t.entity = Ecs%s,\n\t\t\t.constants = {\n", decl->name);

                for (auto constant : enum_decl->constants) {
                    if (underlying.is_unsigned) {
                        file_writef(&f, "\t\t\t\t{ .name = \"%s\", .value_unsigned = %llu },\n", constant->name, (unsigned long long)constant->value);
                    } else {
                        file_writef(&f, "\t\t\t\t{ .name = \"%s\", .value = %lld },\n", constant->name, constant->value);
                    }
                }

                file_writef(&f, "\t\t\t},\n\t\t\t.underlying_type = ecs_id(%s),\n\t\t};\n", underlying.name);
                file_writef(&f, "\t\tecs_id(%s) = ecs_enum_init(ecs, &desc);\n", decl->name);

                for (auto constant : enum_decl->constants) {
                    file_writef(&f, "\t\tecs_id(%s) = ecs_lookup_child(ecs, Ecs%s, \"%s\");\n", constant->name, decl->name, constant->name);
                }

                file_write(&f, "\t}\n");
                if (decl->next) file_write(&f, "\n");
            }

            if (flecs_component_decls && (flecs_enum_tag_decls || flecs_tag_decls)) file_write(&f, "\n");

            if (flecs_component_decls) {
                for (auto decl : flecs_component_decls) {
                    file_writef(&f, "\tECS_COMPONENT_DEFINE(ecs, %s);\n", decl->name);
                }
            }

            if (flecs_enum_tag_decls || flecs_component_decls) {
                file_write(&f, "\n#ifdef __cplusplus__\n");
                for (auto decl : flecs_enum_tag_decls) {
                    file_writef(&f, "\tecs.component<%s>(nullptr, true, Ecs%s);\n", decl->name, decl->name);
                }
                for (auto decl : flecs_component_decls) {
                    file_writef(&f, "\tecs.component<%s>();\n", decl->name);
                }
                file_write(&f, "#endif // __cplusplus__\n");
            }


            if (flecs_module_decls) file_write(&f, "\n\tecs_set_scope(ecs, prev_scope);\n");
            file_write(&f, "}\n");

            if (generate_flecs_meta) {
                file_writef(&f, "\nvoid flecs_register_%.*s_meta(flecs::world &ecs)\n{\n", src_name_len, src_filename);
                if (flecs_module_decls) {
                    file_writef(&f, "\textern ECS_COMPONENT_DECLARE(%s);\n", flecs_module_decls.head.next->name);
                    file_writef(&f, "\tecs_entity_t prev_scope = ecs_set_scope(ecs, ecs_id(%s));\n\n", flecs_module_decls.head.next->name);
                }

                for (auto decl : flecs_tag_decls) {
                    if (!decl->args) continue;
                    file_writef(&f, "\t// %s\n", decl->name);
                    for (auto arg : decl->args) {
                        char entity[4096];
                        snprintf(entity, sizeof entity, "ecs_id(%s)", decl->name);
                        emit_flecs_add_id(&f, entity, arg);
                    }

                    if (decl->next) file_write(&f, "\n");
                }

                if (flecs_enum_tag_decls && flecs_tag_decls) file_write(&f, "\n");

                for (auto decl : flecs_enum_tag_decls) {
                    if (!decl->args) continue;
                    file_writef(&f, "\t// %s\n", decl->name);

                    for (auto arg : decl->args) {
                        char entity[4096];
                        snprintf(entity, sizeof entity, "Ecs%s", decl->name);
                        emit_flecs_add_id(&f, entity, arg);
                    }

                    if (decl->next) file_write(&f, "\n");
                }

                if (flecs_component_decls && (flecs_enum_tag_decls || flecs_tag_decls)) file_write(&f, "\n");

                for (auto decl : flecs_component_decls) {
                    if (!decl->args) continue;

                    file_writef(&f, "\tecs.component<%s>()", decl->name);
                    for (auto arg : decl->args) {
                        if (arg->second) file_writef(&f, "\n\t\t.add(ecs_pair(%s, %s))", arg->name, arg->second);
                        else file_writef(&f, "\n\t\t.add(%s)", arg->name);
                    }

                    file_write(&f, ";\n");
                    if (decl->next) file_write(&f, "\n");
                }

                if (flecs_component_decls && has_flecs_component_decl_meta()) file_write(&f, "\n");

                for (auto decl : flecs_component_decls) {
                    auto *struct_decl = list_find(&struct_decls, decl->name);
                    if (!struct_decl) ERROR(cursor, "no struct or enum decl for component: %s", decl->name);

                    file_writef(&f, "\tecs.component<%s>()", decl->name);
                    emit_flecs_component_members(&f, struct_decl);
                    file_write(&f, ";\n");
                    if (decl->next) file_write(&f, "\n");
                }

                if (flecs_component_decls && has_flecs_meta()) file_write(&f, "\n");

                for (auto decl : flecs_component_decls) {
                    if (auto *struct_decl = list_find(&struct_decls, decl->name);
                        struct_decl && has_flecs_meta(struct_decl))
                    {
                        emit_flecs_meta(&f, struct_decl);
                    }
                }

                if (flecs_module_decls) file_write(&f, "\n\tecs_set_scope(ecs, prev_scope);\n");
                file_write(&f, "}\n");
            }

            file_writef(&f, "\n#endif // %s_GENERATED_IMPL\n", name);
        }
    }

    if (generate_tests) {
        char tests_out_path[4096];
        snprintf(tests_out_path, sizeof tests_out_path, "%s/tests", out_path);
        std::filesystem::create_directories(tests_out_path);

        char path[4096];
        snprintf(path, sizeof path, "%s/%.*s.h", tests_out_path, src_name_len, src_filename);

        HashedFile f{};
        XXH3_INITSTATE(&f.hash);
        XXH3_128bits_reset_withSeed(&f.hash, META_VERSION);

        defer {
            XXH128_hash_t hash = XXH3_128bits_digest(&f.hash);
            XXH128_hash_t curr = hash_file_on_disk(path);

            if (!XXH128_isEqual(hash, curr)) {
                if (FILE *fp = fopen(path, "wb")) {
                    for (auto *it = &f.stream.head; it; it = it->next) {
                        fwrite(it->data, 1, it->count, fp);
                    }
                    fclose(fp);
                } else {
                    FERROR("failed to open file '%s': %s\n", path, strerror(errno));
                }
            }
        };

        emit_include_guard_begin(&f, nullptr, name, "TEST");
        defer { emit_include_guard_end(&f, nullptr, name, "TEST"); };

        for (auto decl : test_proc_decls) {
            emit_proc_decl(&f , tu, decl->cursor, decl->attributes);
        }
        file_write(&f, "\n");

        DynamicArray<char*> categories{};
        array_add(&categories, (char*)"");

        DynamicArray<DynamicArray<ProcDecl>> procs{};
        array_add(&procs, {});

        for (auto decl : test_proc_decls) {
            const char *test_name = decl->name;
            for (const char *it = strchr(decl->name, '_'); it; it = strchr(it+1, '_')) {
                if (it[1] == '_' && it[2]) {
                    test_name = it+2;
                    it += 1;
                }
            }

            if (test_name != decl->name) {
                size_t ci = (size_t)test_name - (size_t)decl->name - 2;
                char c = decl->name[ci];
                decl->name[ci] = '\0';

                char *category = decl->name;
                int idx = array_find(&categories, category);
                if (idx == -1) idx = array_add(&categories, strdup(category));
                decl->name[ci] = c;

                if (idx >= procs.count) array_add(&procs, {});
                array_add(&procs[idx], ProcDecl(decl));
            } else {
                array_add(&procs[0], ProcDecl(decl));
            }
        }

        for (int i = 1; i < categories.count; i++) {
            char *category = categories[i];
            file_writef(&f, "TestSuite %s__%s__tests[] = {\n", name, category);

            for (auto decl : procs[i]) {
                const char *test_name = decl.name;
                for (const char *it = strchr(decl.name, '_'); it; it = strchr(it+1, '_')) {
                    if (it[1] == '_' && it[2]) {
                        test_name = it+2;
                        it += 1;
                    }
                }

                file_writef(&f, "\t{ \"%s\", %s },\n", test_name, decl.name);
            }

            file_write(&f, "};\n\n");
        }

        qsort(
            categories.data, categories.count, sizeof *categories.data,
            [](const void *lhs, const void *rhs) -> int
            {
                return strcmp(*(char**)lhs, *(char**)rhs);
            });


        file_writef(&f, "TestSuite %s__tests[] = {\n", name);
        for (int i = 0; i < categories.count; i++) {
            char *category = categories[i];
            if (category && *category) {
                file_write(&f, "\t{ \"");
                for (char *it = category; *it; it++) {
                    if (*it == '_' && *(it+1) == '_') {
                        file_writec(&f, '/');
                        it++;
                    } else file_writec(&f, *it);
                }
                file_write(&f, "\"");

                file_writef(
                    &f,
                    ", nullptr, %s__%s__tests, sizeof(%s__%s__tests)/sizeof(%s__%s__tests[0]) },\n",
                    name, category,
                    name, category,
                    name, category);
            } else {
                for (auto decl : procs[i]) {
                    const char *test_name = decl.name;
                    for (const char *it = strchr(decl.name, '_'); it; it = strchr(it+1, '_')) {
                        if (it[1] == '_' && it[2]) {
                            test_name = it+2;
                            it += 1;
                        }
                    }

                    file_writef(&f, "\t{ \"%s\", %s },\n", test_name, decl.name);
                }
            }
        }
        file_write(&f, "};\n");
    }

    if (generate_integration_tests) {
        char tests_out_path[4096];
        snprintf(tests_out_path, sizeof tests_out_path, "%s/tests", out_path);
        std::filesystem::create_directories(tests_out_path);

        char path[4096];
        snprintf(path, sizeof path, "%s/%.*s.h", tests_out_path, src_name_len, src_filename);

        HashedFile f{};
        XXH3_INITSTATE(&f.hash);
        XXH3_128bits_reset_withSeed(&f.hash, META_VERSION);

        defer {
            XXH128_hash_t hash = XXH3_128bits_digest(&f.hash);
            XXH128_hash_t curr = hash_file_on_disk(path);

            if (!XXH128_isEqual(hash, curr)) {
                if (FILE *fp = fopen(path, "wb")) {
                    for (auto *it = &f.stream.head; it; it = it->next) {
                        fwrite(it->data, 1, it->count, fp);
                    }
                    fclose(fp);
                } else {
                    FERROR("failed to open file '%s': %s\n", path, strerror(errno));
                }
            }
        };

        emit_include_guard_begin(&f, nullptr, name, "INTEGRATION_TEST");
        defer { emit_include_guard_end(&f, nullptr, name, "INTEGRATION_TEST"); };

        for (auto decl : integration_test_proc_decls) {
            emit_proc_decl(&f, tu, decl->cursor, decl->attributes);
        }
        file_write(&f, "\n");

        file_writef(&f, "TestSuite %s__integration_tests[] = {\n", name);
        for (auto decl : integration_test_proc_decls) {
            file_writef(&f, "\t{ \"%s\", %s, nullptr, 0, true },\n", decl->name, decl->name);
        }
        file_write(&f, "};\n");
    }

    if (opts.depfile && includes) {
        const char *path = opts.depfile;

        HashedFile f{};
        XXH3_INITSTATE(&f.hash);
        XXH3_128bits_reset(&f.hash);

        defer {
            if (FILE *fp = fopen(path, "wb")) {
                for (auto *it = &f.stream.head; it; it = it->next) {
                    fwrite(it->data, 1, it->count, fp);
                }

                fclose(fp);
            } else {
                FERROR("failed to open out.file '%s': %s\n", path, strerror(errno));
            }
        };

        char h_path[4096];
        snprintf(h_path, sizeof h_path, "%s/%.*s.h", out_path, src_name_len, src_filename);
        file_writef(&f, "%s: \\\n", h_path);

        for (auto it : includes) {
            CXString file_s = clang_getFileName(it->file);
            if (clang_String_isNull(file_s)) continue;

            const char *file_sz = clang_getCString(file_s);
            if (!file_sz) continue;
            defer { clang_disposeString(file_s); };

            if (strcmp(file_sz, h_path) == 0) continue;

            file_writef(&f, "  %s \\\n", file_sz);
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
            if (argv[i][1] == '-') {
                char *p = &argv[i][2];
                if (strcmp(p, "trace-cursor") == 0) {
                    debug_trace_cursor = argv[++i];
                    printf("trace-file: '%s'\n", debug_trace_cursor);
                } else if (strcmp(p, "trace-file") == 0) {
                    debug_trace_file = argv[++i];
                    printf("trace-file: '%s'\n", debug_trace_file);
                } else if (strcmp(p, "depfile") == 0) {
                    opts.depfile = argv[++i];
                } else if (*p == '\0') {
                    fargc = argc - i - 1;
                    fargv = argv + i + 1;
                    break;
                } else {
                    printf("unhandled argv[%d]:%s\n", i, argv[i]);
                }
            } else if (argv[i][1] == 'o') {
                out_path = argv[++i];
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
    defer { clang_disposeIndex(index); };

    unsigned int flags =
        CXTranslationUnit_SkipFunctionBodies |
        CXTranslationUnit_DetailedPreprocessingRecord |
        CXTranslationUnit_KeepGoing |
        0;

    CXTranslationUnit tu;
    CXErrorCode result = clang_parseTranslationUnit2(index, src_filename, fargv, fargc, nullptr, 0, flags, &tu);
    defer { clang_disposeTranslationUnit(tu); };

    if (result) {
        const char *sz_result = "";
        switch (result) {
        case CXError_Success: break;
        case CXError_Failure: sz_result = "Failure"; break;
        case CXError_Crashed: sz_result = "Crashed"; break;
        case CXError_InvalidArguments: sz_result = "InvalidArguments"; break;
        case CXError_ASTReadError: sz_result = "ASTReadError"; break;
        }

        FERROR("failed to parse translation unit: '%s'\n", sz_result);
        return 1;
    }

    if (!generate_header(out_path, src_filename, tu)) return 1;
    return 0;
}
