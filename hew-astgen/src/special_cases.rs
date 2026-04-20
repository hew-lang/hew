use std::sync::OnceLock;

const CURRENT_SCHEMA_VERSION: u32 = hew_serialize::msgpack::SCHEMA_VERSION;

/// Hard-coded C++ parser for Literal (custom serde, not derive(Serialize)).
pub fn literal_parser() -> &'static str {
    r#"static ast::Literal parseLiteral(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);
  if (name == "Integer")
    return ast::LitInteger{getInt(*payload)};
  if (name == "Float")
    return ast::LitFloat{getFloat(*payload)};
  if (name == "String")
    return ast::LitString{getString(*payload)};
  if (name == "Bool")
    return ast::LitBool{getBool(*payload)};
  if (name == "Char") {
    // Rust char serializes as a UTF-8 string — decode full codepoint
    auto s = getString(*payload);
    if (s.empty())
      return ast::LitChar{0};
    auto c = static_cast<unsigned char>(s[0]);
    char32_t cp;
    if (c < 0x80) {
      cp = c;
    } else if ((c >> 5) == 0x6 && s.size() >= 2) {
      cp = (char32_t(c & 0x1F) << 6) | (s[1] & 0x3F);
    } else if ((c >> 4) == 0xE && s.size() >= 3) {
      cp = (char32_t(c & 0x0F) << 12) | (char32_t(s[1] & 0x3F) << 6) | (s[2] & 0x3F);
    } else if ((c >> 3) == 0x1E && s.size() >= 4) {
      cp = (char32_t(c & 0x07) << 18) | (char32_t(s[1] & 0x3F) << 12) |
           (char32_t(s[2] & 0x3F) << 6) | (s[3] & 0x3F);
    } else {
      cp = c; // fallback to raw byte
    }
    return ast::LitChar{cp};
  }
  if (name == "Duration")
    return ast::LitDuration{getInt(*payload)};
  fail("unknown Literal variant: " + name);
}"#
}

/// Hard-coded parser for `AttributeArg` because the C++ AST deliberately stores
/// only the positional and key/value forms.
pub fn attribute_arg_parser() -> &'static str {
    r#"static ast::AttributeArg parseAttributeArg(const msgpack::object &obj) {
  auto [variant, payload] = getEnumVariant(obj);
  if (variant == "Positional" && payload) {
    return ast::AttributeArg{ast::AttributeArgPositional{getString(*payload)}};
  }
  if (variant == "KeyValue" && payload) {
    ast::AttributeArgKeyValue kv;
    kv.key = getString(mapReq(*payload, "key"));
    kv.value = getString(mapReq(*payload, "value"));
    return ast::AttributeArg{std::move(kv)};
  }
  fail("unknown AttributeArg variant: " + variant);
}"#
}

/// Hard-coded parser for Attribute so special-cased receive parsing can reuse
/// the same C++ representation without exposing a Duration variant.
pub fn attribute_parser() -> &'static str {
    r#"static ast::Attribute parseAttribute(const msgpack::object &obj) {
  ast::Attribute attr;
  attr.name = getString(mapReq(obj, "name"));
  attr.args = parseVec<ast::AttributeArg>(mapReq(obj, "args"), parseAttributeArg);
  attr.span = parseSpan(mapReq(obj, "span"));
  return attr;
}"#
}

/// Hard-coded C++ parser for `ExprTypeEntry` (C++-only type from serialization layer).
pub fn expr_type_entry_parser() -> &'static str {
    r#"static ast::ExprTypeEntry parseExprTypeEntry(const msgpack::object &obj) {
  ast::ExprTypeEntry entry;
  entry.start = getUint(mapReq(obj, "start"));
  entry.end = getUint(mapReq(obj, "end"));
  entry.ty = parseSpanned<ast::TypeExpr>(mapReq(obj, "ty"), parseTypeExpr);
  return entry;
}"#
}

/// Hard-coded parser for the serialized assignment-target kind tag.
pub fn assign_target_kind_data_parser() -> &'static str {
    r#"static ast::AssignTargetKindData parseAssignTargetKindData(const msgpack::object &obj) {
  auto kind = getString(obj);
  if (kind == "local_var")
    return ast::AssignTargetKindLocalVar{};
  if (kind == "actor_field")
    return ast::AssignTargetKindActorField{};
  if (kind == "field_access")
    return ast::AssignTargetKindFieldAccess{};
  if (kind == "index")
    return ast::AssignTargetKindIndex{};
  fail("unknown AssignTargetKindData variant: " + kind);
}"#
}

/// Hard-coded parser for `AssignTargetKindEntry` (C++-only type from serialization layer).
pub fn assign_target_kind_entry_parser() -> &'static str {
    r#"static ast::AssignTargetKindEntry parseAssignTargetKindEntry(const msgpack::object &obj) {
  ast::AssignTargetKindEntry entry;
  entry.start = getUint(mapReq(obj, "start"));
  entry.end = getUint(mapReq(obj, "end"));
  entry.kind = parseAssignTargetKindData(mapReq(obj, "kind"));
  return entry;
}"#
}

/// Hard-coded parser for `AssignTargetShapeEntry` (C++-only type from serialization layer).
pub fn assign_target_shape_entry_parser() -> &'static str {
    r#"static ast::AssignTargetShapeEntry parseAssignTargetShapeEntry(const msgpack::object &obj) {
  ast::AssignTargetShapeEntry entry;
  entry.start = getUint(mapReq(obj, "start"));
  entry.end = getUint(mapReq(obj, "end"));
  entry.is_unsigned = getBool(mapReq(obj, "is_unsigned"));
  return entry;
}"#
}

/// Hard-coded parser for `LoweringFactEntry` and its enum tags (C++-only types from serialization layer).
pub fn lowering_fact_entry_parser() -> &'static str {
    r#"static ast::LoweringKind parseLoweringKind(const msgpack::object &obj) {
  auto kind = getString(obj);
  if (kind == "hash_set")
    return ast::LoweringKind::HashSet;
  fail("unknown lowering kind '" + kind + "'");
}

static ast::HashSetElementType parseHashSetElementType(const msgpack::object &obj) {
  auto kind = getString(obj);
  if (kind == "i64")
    return ast::HashSetElementType::I64;
  if (kind == "u64")
    return ast::HashSetElementType::U64;
  if (kind == "str")
    return ast::HashSetElementType::Str;
  fail("unknown HashSet element type '" + kind + "'");
}

static ast::HashSetAbi parseHashSetAbi(const msgpack::object &obj) {
  auto abi = getString(obj);
  if (abi == "int64")
    return ast::HashSetAbi::Int64;
  if (abi == "string")
    return ast::HashSetAbi::String;
  fail("unknown HashSet ABI '" + abi + "'");
}

static ast::DropKind parseDropKind(const msgpack::object &obj) {
  auto drop = getString(obj);
  if (drop == "hash_set_free")
    return ast::DropKind::HashSetFree;
  fail("unknown drop kind '" + drop + "'");
}

static ast::LoweringFactEntry parseLoweringFactEntry(const msgpack::object &obj) {
  ast::LoweringFactEntry entry;
  entry.start = getUint(mapReq(obj, "start"));
  entry.end = getUint(mapReq(obj, "end"));
  entry.kind = parseLoweringKind(mapReq(obj, "kind"));
  entry.element_type = parseHashSetElementType(mapReq(obj, "element_type"));
  entry.abi_variant = parseHashSetAbi(mapReq(obj, "abi_variant"));
  entry.drop_kind = parseDropKind(mapReq(obj, "drop_kind"));
  return entry;
}"#
}

/// Hard-coded parser for `MethodCallReceiverKindEntry` (C++-only type from serialization layer).
pub fn method_call_receiver_kind_entry_parser() -> &'static str {
    r#"static ast::MethodCallReceiverKindEntry
parseMethodCallReceiverKindEntry(const msgpack::object &obj) {
  ast::MethodCallReceiverKindEntry entry;
  entry.start = getUint(mapReq(obj, "start"));
  entry.end = getUint(mapReq(obj, "end"));
  auto kind = getString(mapReq(obj, "kind"));
  if (kind == "named_type_instance") {
    ast::MethodCallReceiverKindNamedTypeInstance data;
    data.type_name = getString(mapReq(obj, "type_name"));
    entry.kind = std::move(data);
    return entry;
  }
  if (kind == "handle_instance") {
    ast::MethodCallReceiverKindHandleInstance data;
    data.type_name = getString(mapReq(obj, "type_name"));
    entry.kind = std::move(data);
    return entry;
  }
  if (kind == "trait_object") {
    ast::MethodCallReceiverKindTraitObject data;
    data.trait_name = getString(mapReq(obj, "trait_name"));
    entry.kind = std::move(data);
    return entry;
  }
  if (kind == "stream_instance") {
    ast::MethodCallReceiverKindStreamInstance data;
    data.element_kind = getString(mapReq(obj, "element_kind"));
    entry.kind = std::move(data);
    return entry;
  }
  fail("unknown method_call_receiver_kinds kind '" + kind + "'");
}"#
}

/// Hard-coded parser for `ModuleId` to preserve string-key compatibility in
/// `ModuleGraph.modules`.
pub fn module_id_parser() -> &'static str {
    r#"static ast::ModuleId parseModuleId(const msgpack::object &obj) {
  ast::ModuleId id;
  // JSON input: module IDs are serialized as flat strings (e.g., "std::misc::log")
  // because JSON objects require string keys. Split on "::" to recover path segments.
  if (obj.type == msgpack::type::STR) {
    auto s = getString(obj);
    if (s == "(root)") {
      // Root module has an empty path.
      return id;
    }
    // Split on "::"
    size_t pos = 0;
    while (pos < s.size()) {
      auto next = s.find("::", pos);
      if (next == std::string::npos) {
        id.path.push_back(s.substr(pos));
        break;
      }
      id.path.push_back(s.substr(pos, next - pos));
      pos = next + 2;
    }
    return id;
  }
  // Msgpack input: module IDs are serialized as {"path": ["seg1", "seg2", ...]}.
  id.path = parseVec<std::string>(mapReq(obj, "path"),
                                  [](const msgpack::object &o) { return getString(o); });
  return id;
}"#
}

pub fn module_import_parser() -> &'static str {
    r#"static ast::ModuleImport parseModuleImport(const msgpack::object &obj) {
  ast::ModuleImport mi;
  mi.target = parseModuleId(mapReq(obj, "target"));
  const auto *spec = mapGet(obj, "spec");
  if (spec && !isNil(*spec)) {
    auto [name, payload] = getEnumVariant(*spec);
    if (name == "Glob") {
      mi.spec = ast::ImportSpecGlob{};
    } else if (name == "Names") {
      mi.spec =
          ast::ImportSpecNames{parseVec<ast::ImportName>(*payload, [](const msgpack::object &o) {
            ast::ImportName n;
            n.name = getString(mapReq(o, "name"));
            const auto *alias = mapGet(o, "alias");
            if (alias && !isNil(*alias))
              n.alias = getString(*alias);
            return n;
          })};
    }
  }
  mi.span = parseSpan(mapReq(obj, "span"));
  return mi;
}"#
}

pub fn module_parser() -> &'static str {
    r#"static ast::Module parseModule(const msgpack::object &obj) {
  ast::Module m;
  m.id = parseModuleId(mapReq(obj, "id"));
  m.items = parseVec<ast::Spanned<ast::Item>>(mapReq(obj, "items"), [](const msgpack::object &o) {
    return parseSpanned<ast::Item>(o, parseItem);
  });
  m.imports = parseVec<ast::ModuleImport>(mapReq(obj, "imports"), parseModuleImport);
  const auto *sp = mapGet(obj, "source_paths");
  if (sp && !isNil(*sp)) {
    m.source_paths =
        parseVec<std::string>(*sp, [](const msgpack::object &o) { return getString(o); });
  }
  const auto *doc = mapGet(obj, "doc");
  if (doc && !isNil(*doc))
    m.doc = getString(*doc);
  return m;
}"#
}

/// Hard-coded C++ parser for `ModuleGraph` (`HashMap`<`ModuleId`, Module> with custom hasher).
pub fn module_graph_parser() -> &'static str {
    r#"static ast::ModuleGraph parseModuleGraph(const msgpack::object &obj) {
  ast::ModuleGraph mg;
  mg.root = parseModuleId(mapReq(obj, "root"));
  mg.topo_order = parseVec<ast::ModuleId>(mapReq(obj, "topo_order"), parseModuleId);

  const auto &modulesObj = mapReq(obj, "modules");
  if (modulesObj.type == msgpack::type::ARRAY) {
    for (uint32_t i = 0; i < modulesObj.via.array.size; ++i) {
      const auto &pair = modulesObj.via.array.ptr[i];
      if (pair.type == msgpack::type::ARRAY && pair.via.array.size == 2) {
        auto id = parseModuleId(pair.via.array.ptr[0]);
        auto mod = parseModule(pair.via.array.ptr[1]);
        mg.modules.emplace(std::move(id), std::move(mod));
      }
    }
  } else if (modulesObj.type == msgpack::type::MAP) {
    for (uint32_t i = 0; i < modulesObj.via.map.size; ++i) {
      auto id = parseModuleId(modulesObj.via.map.ptr[i].key);
      auto mod = parseModule(modulesObj.via.map.ptr[i].val);
      mg.modules.emplace(std::move(id), std::move(mod));
    }
  }
  return mg;
}"#
}

/// Hard-coded C++ parser for Program (wraps `TypedProgram` with extra fields).
pub fn program_parser() -> &'static str {
    r#"static ast::Program parseProgram(const msgpack::object &obj) {
  ast::Program prog;

  // The embedded msgpack boundary is internal to the current `hew` binary, so
  // require an explicit, exact schema version instead of carrying fallback
  // decoding for older payloads.
  prog.schema_version = getUint32(mapReq(obj, "schema_version"), "schema_version");
  if (prog.schema_version != CURRENT_SCHEMA_VERSION) {
    fail("unsupported schema version " + std::to_string(prog.schema_version) +
         " (expected: " + std::to_string(CURRENT_SCHEMA_VERSION) + ")");
  }

  prog.items =
      parseVec<ast::Spanned<ast::Item>>(mapReq(obj, "items"), [](const msgpack::object &o) {
        return parseSpanned<ast::Item>(o, parseItem);
      });
  const auto *md = mapGet(obj, "module_doc");
  if (md && !isNil(*md))
    prog.module_doc = getString(*md);
  prog.expr_types = parseVec<ast::ExprTypeEntry>(mapReq(obj, "expr_types"), parseExprTypeEntry);
  prog.method_call_receiver_kinds = parseVec<ast::MethodCallReceiverKindEntry>(
      mapReq(obj, "method_call_receiver_kinds"), parseMethodCallReceiverKindEntry);
  prog.assign_target_kinds = parseVec<ast::AssignTargetKindEntry>(
      mapReq(obj, "assign_target_kinds"), parseAssignTargetKindEntry);
  prog.assign_target_shapes = parseVec<ast::AssignTargetShapeEntry>(
      mapReq(obj, "assign_target_shapes"), parseAssignTargetShapeEntry);
  prog.lowering_facts =
      parseVec<ast::LoweringFactEntry>(mapReq(obj, "lowering_facts"), parseLoweringFactEntry);

  // Handle type metadata: list of known handle type names
  prog.handle_types =
      parseVec<std::string>(mapReq(obj, "handle_types"), [](const msgpack::object &o) {
        return getString(o);
      });
  prog.handle_bearing_structs =
      parseVec<std::string>(mapReq(obj, "handle_bearing_structs"), [](const msgpack::object &o) {
        return getString(o);
      });

  // Handle type representations: map of type name → repr string ("i32", etc.)
  const auto &hr = mapReq(obj, "handle_type_repr");
  if (hr.type != msgpack::type::MAP)
    fail("expected map, got type " + std::to_string(hr.type));
  for (uint32_t i = 0; i < hr.via.map.size; ++i) {
    auto &kv = hr.via.map.ptr[i];
    std::string key = getString(kv.key);
    std::string val = getString(kv.val);
    prog.handle_type_repr[key] = val;
  }
  const auto *mg = mapGet(obj, "module_graph");
  if (mg && !isNil(*mg))
    prog.module_graph = parseModuleGraph(*mg);

  // Source path for debug info (optional — some frontend flows do not provide it)
  const auto *sp = mapGet(obj, "source_path");
  if (sp && !isNil(*sp))
    prog.source_path = getString(*sp);

  // Line map: byte offset of each line start (optional)
  const auto *lm = mapGet(obj, "line_map");
  if (lm && !isNil(*lm))
    prog.line_map = parseVec<size_t>(
        *lm, [](const msgpack::object &o) { return static_cast<size_t>(getUint(o)); });

  // Drop function metadata: maps qualified handle type name → C drop function.
  // Populated from `impl Drop` blocks in stdlib .hew files.
  // Fail closed: if the key is present it must be a map of string pairs.
  const auto *df = mapGet(obj, "drop_funcs");
  if (df && !isNil(*df)) {
    if (df->type != msgpack::type::ARRAY)
      fail("expected array for drop_funcs, got type " + std::to_string(df->type));
    for (uint32_t i = 0; i < df->via.array.size; ++i) {
      const auto &pair = df->via.array.ptr[i];
      if (pair.type != msgpack::type::ARRAY || pair.via.array.size != 2)
        fail("drop_funcs entry must be a 2-element array");
      std::string ty = getString(pair.via.array.ptr[0]);
      std::string func = getString(pair.via.array.ptr[1]);
      prog.drop_funcs[ty] = func;
    }
  }

  return prog;
}"#
}

/// Hard-coded parser for `TypeDecl` (has `method_storage` ownership pattern).
pub fn type_decl_parser() -> &'static str {
    r#"static ast::TypeDecl parseTypeDecl(const msgpack::object &obj) {
  ast::TypeDecl td;
  const auto *vis = mapGet(obj, "visibility");
  if (vis && !isNil(*vis))
    td.visibility = parseVisibility(*vis);
  auto kindStr = getString(mapReq(obj, "kind"));
  if (kindStr == "Enum")
    td.kind = ast::TypeDeclKind::Enum;
  else if (kindStr == "Struct")
    td.kind = ast::TypeDeclKind::Struct;
  else
    fail("unknown TypeDeclKind: " + kindStr);
  td.name = getString(mapReq(obj, "name"));
  const auto *tp = mapGet(obj, "type_params");
  if (tp && !isNil(*tp))
    td.type_params = parseVec<ast::TypeParam>(*tp, parseTypeParam);
  const auto *wc = mapGet(obj, "where_clause");
  if (wc && !isNil(*wc))
    td.where_clause = parseWhereClause(*wc);
  td.body = parseVec<ast::TypeBodyItem>(mapReq(obj, "body"), [&td](const msgpack::object &o) {
    auto [name, payload] = getEnumVariant(o);
    if (name == "Field") {
      ast::TypeBodyItemField f;
      f.name = getString(mapReq(*payload, "name"));
      f.ty = parseSpanned<ast::TypeExpr>(mapReq(*payload, "ty"), parseTypeExpr);
      return ast::TypeBodyItem{std::move(f)};
    }
    if (name == "Variant") {
      return ast::TypeBodyItem{ast::TypeBodyVariant{parseVariantDecl(*payload)}};
    }
    if (name == "Method") {
      auto fn = std::make_unique<ast::FnDecl>(parseFnDecl(*payload));
      ast::TypeBodyMethod m;
      m.fn = fn.get();
      td.method_storage.push_back(std::move(fn));
      return ast::TypeBodyItem{std::move(m)};
    }
    fail("unknown TypeBodyItem variant: " + name);
  });
  const auto *dc = mapGet(obj, "doc_comment");
  if (dc && !isNil(*dc))
    td.doc_comment = getString(*dc);
  const auto *w = mapGet(obj, "wire");
  if (w && !isNil(*w))
    td.wire = parseWireMetadata(*w);
  const auto *ind = mapGet(obj, "is_indirect");
  if (ind && !isNil(*ind))
    td.is_indirect = getBool(*ind);
  return td;
}"#
}

/// Hard-coded parser for `VariantDecl` (kind uses nested C++ types).
pub fn variant_decl_parser() -> &'static str {
    r#"static ast::VariantDecl parseVariantDecl(const msgpack::object &obj) {
  ast::VariantDecl result;
  result.name = getString(mapReq(obj, "name"));

  const auto &kindObj = mapReq(obj, "kind");
  auto [name, payload] = getEnumVariant(kindObj);
  if (name == "Unit") {
    result.kind = ast::VariantDecl::VariantUnit{};
    return result;
  }
  if (name == "Tuple") {
    ast::VariantDecl::VariantTuple tuple;
    tuple.fields = parseVec<ast::Spanned<ast::TypeExpr>>(
        *payload, [](const msgpack::object &o) { return parseSpanned<ast::TypeExpr>(o, parseTypeExpr); });
    result.kind = std::move(tuple);
    return result;
  }
  if (name == "Struct") {
    ast::VariantDecl::VariantStruct s;
    s.fields = parseVec<ast::VariantDecl::VariantStructField>(
        *payload, [](const msgpack::object &o) {
          uint32_t sz;
          const auto *arr = arrayData(o, sz);
          if (sz != 2)
            fail("tuple should have 2 elements");
          ast::VariantDecl::VariantStructField field;
          field.name = getString(arr[0]);
          field.ty = parseSpanned<ast::TypeExpr>(arr[1], parseTypeExpr);
          return field;
        });
    result.kind = std::move(s);
    return result;
  }
  fail("unknown VariantKind variant: " + name);
}"#
}

/// Hard-coded parser for `TraitItem` (header uses `default_value` field name).
pub fn trait_item_parser() -> &'static str {
    r#"static ast::TraitItem parseTraitItem(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);

  if (name == "Method")
    return ast::TraitItem{ast::TraitItemMethod{parseTraitMethod(*payload)}};
  if (name == "AssociatedType") {
    ast::TraitItemAssociatedType e;
    e.name = getString(mapReq(*payload, "name"));
    e.bounds = parseVec<ast::TraitBound>(mapReq(*payload, "bounds"), parseTraitBound);
    const auto *default_value = mapGet(*payload, "default");
    if (default_value && !isNil(*default_value))
      e.default_value = parseSpanned<ast::TypeExpr>(*default_value, parseTypeExpr);
    return ast::TraitItem{std::move(e)};
  }
  fail("unknown TraitItem variant: " + name);
}"#
}

/// Hard-coded parser lifted from the working C++ reader.
pub fn match_arm_parser() -> &'static str {
    r#"static ast::MatchArm parseMatchArm(const msgpack::object &obj) {
  ast::MatchArm arm;
  arm.pattern = parseSpanned<ast::Pattern>(mapReq(obj, "pattern"), parsePattern);
  const auto *g = mapGet(obj, "guard");
  if (g && !isNil(*g))
    arm.guard = parseSpannedPtr<ast::Expr>(*g, parseExpr);
  arm.body = parseSpannedPtr<ast::Expr>(mapReq(obj, "body"), parseExpr);
  return arm;
}"#
}

/// Hard-coded parser lifted from the working C++ reader.
pub fn select_arm_parser() -> &'static str {
    r#"static ast::SelectArm parseSelectArm(const msgpack::object &obj) {
  ast::SelectArm arm;
  arm.binding = parseSpanned<ast::Pattern>(mapReq(obj, "binding"), parsePattern);
  arm.source = parseSpannedPtr<ast::Expr>(mapReq(obj, "source"), parseExpr);
  arm.body = parseSpannedPtr<ast::Expr>(mapReq(obj, "body"), parseExpr);
  return arm;
}"#
}

/// Hard-coded parser lifted from the working C++ reader.
#[allow(
    dead_code,
    reason = "coverage enum reserves explicit rejection slots for explicit rejections"
)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VariantDisposition {
    Parsed,
    Rejected,
}

const EXPR_VARIANT_COVERAGE: &[(&str, VariantDisposition)] = &[
    ("Binary", VariantDisposition::Parsed),
    ("Unary", VariantDisposition::Parsed),
    ("Literal", VariantDisposition::Parsed),
    ("Identifier", VariantDisposition::Parsed),
    ("Tuple", VariantDisposition::Parsed),
    ("Array", VariantDisposition::Parsed),
    ("ArrayRepeat", VariantDisposition::Parsed),
    ("MapLiteral", VariantDisposition::Parsed),
    ("Block", VariantDisposition::Parsed),
    ("If", VariantDisposition::Parsed),
    ("IfLet", VariantDisposition::Parsed),
    ("Match", VariantDisposition::Parsed),
    ("Lambda", VariantDisposition::Parsed),
    ("Spawn", VariantDisposition::Parsed),
    ("SpawnLambdaActor", VariantDisposition::Parsed),
    ("Scope", VariantDisposition::Parsed),
    ("InterpolatedString", VariantDisposition::Parsed),
    ("Call", VariantDisposition::Parsed),
    ("MethodCall", VariantDisposition::Parsed),
    ("StructInit", VariantDisposition::Parsed),
    ("Send", VariantDisposition::Parsed),
    ("Select", VariantDisposition::Parsed),
    ("Join", VariantDisposition::Parsed),
    ("Timeout", VariantDisposition::Parsed),
    ("Unsafe", VariantDisposition::Parsed),
    ("Yield", VariantDisposition::Parsed),
    ("Cooperate", VariantDisposition::Parsed),
    ("This", VariantDisposition::Parsed),
    ("FieldAccess", VariantDisposition::Parsed),
    ("Index", VariantDisposition::Parsed),
    ("Cast", VariantDisposition::Parsed),
    ("PostfixTry", VariantDisposition::Parsed),
    ("Range", VariantDisposition::Parsed),
    ("Await", VariantDisposition::Parsed),
    ("ScopeLaunch", VariantDisposition::Parsed),
    ("ScopeSpawn", VariantDisposition::Parsed),
    ("ScopeCancel", VariantDisposition::Parsed),
    ("RegexLiteral", VariantDisposition::Parsed),
    ("ByteStringLiteral", VariantDisposition::Parsed),
    ("ByteArrayLiteral", VariantDisposition::Parsed),
];

const STMT_VARIANT_COVERAGE: &[(&str, VariantDisposition)] = &[
    ("Let", VariantDisposition::Parsed),
    ("Var", VariantDisposition::Parsed),
    ("Assign", VariantDisposition::Parsed),
    ("If", VariantDisposition::Parsed),
    ("IfLet", VariantDisposition::Parsed),
    ("Match", VariantDisposition::Parsed),
    ("Loop", VariantDisposition::Parsed),
    ("For", VariantDisposition::Parsed),
    ("While", VariantDisposition::Parsed),
    ("WhileLet", VariantDisposition::Parsed),
    ("Break", VariantDisposition::Parsed),
    ("Continue", VariantDisposition::Parsed),
    ("Return", VariantDisposition::Parsed),
    ("Defer", VariantDisposition::Parsed),
    ("Expression", VariantDisposition::Parsed),
];

fn stitch(parts: &[&str]) -> String {
    let mut out = String::new();
    for part in parts {
        out.push_str(part);
    }
    out
}

fn expr_dispatch_header() -> &'static str {
    r"static ast::Expr parseExpr(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);

"
}

fn expr_scalar_and_collection_dispatcher() -> &'static str {
    r#"  if (name == "Binary") {
    ast::ExprBinary e;
    e.left = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "left"), parseExpr));
    e.op = parseBinaryOp(mapReq(*payload, "op"));
    e.right = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "right"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Unary") {
    ast::ExprUnary e;
    e.op = parseUnaryOp(mapReq(*payload, "op"));
    e.operand = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "operand"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Literal")
    return ast::Expr{ast::ExprLiteral{parseLiteral(*payload)}, {}};
  if (name == "Identifier")
    return ast::Expr{ast::ExprIdentifier{getString(*payload)}, {}};
  if (name == "Tuple") {
    ast::ExprTuple e;
    e.elements = parseVecPtr<ast::Spanned<ast::Expr>>(
        *payload, [](const msgpack::object &o) { return parseSpanned<ast::Expr>(o, parseExpr); });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Array") {
    ast::ExprArray e;
    e.elements = parseVecPtr<ast::Spanned<ast::Expr>>(
        *payload, [](const msgpack::object &o) { return parseSpanned<ast::Expr>(o, parseExpr); });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "MapLiteral") {
    ast::ExprMapLiteral e;
    if (payload) {
      auto entriesArr = mapReq(*payload, "entries");
      if (entriesArr.type == msgpack::type::ARRAY) {
        for (uint32_t i = 0; i < entriesArr.via.array.size; ++i) {
          auto &pairObj = entriesArr.via.array.ptr[i];
          // Each entry is a 2-element array: [key, value]
          if (pairObj.type != msgpack::type::ARRAY || pairObj.via.array.size != 2)
            fail("MapLiteral entry must be a 2-element array, got size " +
                 std::to_string(pairObj.type == msgpack::type::ARRAY ? pairObj.via.array.size : 0));
          ast::ExprMapEntry entry;
          entry.key = std::make_unique<ast::Spanned<ast::Expr>>(
              parseSpanned<ast::Expr>(pairObj.via.array.ptr[0], parseExpr));
          entry.value = std::make_unique<ast::Spanned<ast::Expr>>(
              parseSpanned<ast::Expr>(pairObj.via.array.ptr[1], parseExpr));
          e.entries.emplace_back(std::move(entry));
        }
      }
    }
    return ast::Expr{std::move(e), {}};
  }
  if (name == "ArrayRepeat") {
    ast::ExprArrayRepeat e;
    e.value = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "value"), parseExpr));
    e.count = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "count"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Block")
    return ast::Expr{ast::ExprBlock{parseBlock(*payload)}, {}};
"#
}

fn expr_control_flow_dispatcher() -> &'static str {
    r#"  if (name == "If") {
    ast::ExprIf e;
    e.condition = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "condition"), parseExpr));
    e.then_block = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "then_block"), parseExpr));
    const auto *eb = mapGet(*payload, "else_block");
    if (eb && !isNil(*eb)) {
      e.else_block =
          std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*eb, parseExpr));
    }
    return ast::Expr{std::move(e), {}};
  }
  if (name == "IfLet") {
    ast::ExprIfLet e;
    e.pattern = parseSpanned<ast::Pattern>(mapReq(*payload, "pattern"), parsePattern);
    e.expr = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "expr"), parseExpr));
    e.body = parseBlock(mapReq(*payload, "body"));
    const auto *eb = mapGet(*payload, "else_body");
    if (eb && !isNil(*eb))
      e.else_body = parseBlock(*eb);
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Match") {
    ast::ExprMatch e;
    e.scrutinee = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "scrutinee"), parseExpr));
    e.arms = parseVec<ast::MatchArm>(mapReq(*payload, "arms"), parseMatchArm);
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Lambda") {
    ast::ExprLambda e;
    e.is_move = getBool(mapReq(*payload, "is_move"));
    const auto *tp = mapGet(*payload, "type_params");
    if (tp && !isNil(*tp))
      e.type_params = parseVec<ast::TypeParam>(*tp, parseTypeParam);
    e.params = parseVec<ast::LambdaParam>(mapReq(*payload, "params"), parseLambdaParam);
    const auto *rt = mapGet(*payload, "return_type");
    if (rt && !isNil(*rt))
      e.return_type = parseSpanned<ast::TypeExpr>(*rt, parseTypeExpr);
    e.body = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "body"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
"#
}

fn expr_actor_dispatcher() -> &'static str {
    r#"  if (name == "Spawn") {
    ast::ExprSpawn e;
    e.target = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "target"), parseExpr));
    e.args = parseVec<std::pair<std::string, std::unique_ptr<ast::Spanned<ast::Expr>>>>(
        mapReq(*payload, "args"), [](const msgpack::object &o) {
          // (String, Spanned<Expr>) = [name_str, spanned_expr]
          uint32_t sz;
          const auto *arr = arrayData(o, sz);
          if (sz != 2)
            fail("Spawn arg tuple should have 2 elements");
          return std::make_pair(getString(arr[0]), parseSpannedPtr<ast::Expr>(arr[1], parseExpr));
        });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "SpawnLambdaActor") {
    ast::ExprSpawnLambdaActor e;
    e.is_move = getBool(mapReq(*payload, "is_move"));
    e.params = parseVec<ast::LambdaParam>(mapReq(*payload, "params"), parseLambdaParam);
    const auto *rt = mapGet(*payload, "return_type");
    if (rt && !isNil(*rt))
      e.return_type = parseSpanned<ast::TypeExpr>(*rt, parseTypeExpr);
    e.body = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "body"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Scope") {
    ast::ExprScope e;
    const auto *bind = mapGet(*payload, "binding");
    if (bind && !isNil(*bind))
      e.binding = getString(*bind);
    e.block = parseBlock(mapReq(*payload, "body"));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "InterpolatedString") {
    ast::ExprInterpolatedString e;
    e.parts = parseVec<ast::StringPart>(*payload, parseStringPart);
    return ast::Expr{std::move(e), {}};
  }
"#
}

fn expr_call_dispatcher() -> &'static str {
    r#"  if (name == "Call") {
    ast::ExprCall e;
    e.function = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "function"), parseExpr));
    const auto *ta = mapGet(*payload, "type_args");
    if (ta && !isNil(*ta)) {
      e.type_args = parseVec<ast::Spanned<ast::TypeExpr>>(*ta, [](const msgpack::object &o) {
        return parseSpanned<ast::TypeExpr>(o, parseTypeExpr);
      });
    }
    {
      auto &argsArr = mapReq(*payload, "args");
      if (argsArr.type == msgpack::type::ARRAY) {
        auto &arrData = argsArr.via.array;
        for (uint32_t j = 0; j < arrData.size; j++) {
          e.args.push_back(parseCallArg(arrData.ptr[j]));
        }
      }
    }
    e.is_tail_call = getBool(mapReq(*payload, "is_tail_call"));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "MethodCall") {
    ast::ExprMethodCall e;
    e.receiver = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "receiver"), parseExpr));
    e.method = getString(mapReq(*payload, "method"));
    {
      auto &argsArr = mapReq(*payload, "args");
      if (argsArr.type == msgpack::type::ARRAY) {
        auto &arrData = argsArr.via.array;
        for (uint32_t j = 0; j < arrData.size; j++) {
          e.args.push_back(parseCallArg(arrData.ptr[j]));
        }
      }
    }
    return ast::Expr{std::move(e), {}};
  }
  if (name == "StructInit") {
    ast::ExprStructInit e;
    e.name = getString(mapReq(*payload, "name"));
    e.fields = parseVec<std::pair<std::string, std::unique_ptr<ast::Spanned<ast::Expr>>>>(
        mapReq(*payload, "fields"), [](const msgpack::object &o) {
          // (String, Spanned<Expr>) = [name_str, spanned_expr]
          uint32_t sz;
          const auto *arr = arrayData(o, sz);
          if (sz != 2)
            fail("StructInit field tuple should have 2 elements");
          return std::make_pair(getString(arr[0]), parseSpannedPtr<ast::Expr>(arr[1], parseExpr));
        });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Send") {
    ast::ExprSend e;
    e.target = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "target"), parseExpr));
    e.message = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "message"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
"#
}

fn expr_async_dispatcher() -> &'static str {
    r#"  if (name == "Select") {
    ast::ExprSelect e;
    e.arms = parseVec<ast::SelectArm>(mapReq(*payload, "arms"), parseSelectArm);
    const auto *to = mapGet(*payload, "timeout");
    if (to && !isNil(*to)) {
      e.timeout = std::make_unique<ast::TimeoutClause>(parseTimeoutClause(*to));
    }
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Join") {
    ast::ExprJoin e;
    e.exprs = parseVecPtr<ast::Spanned<ast::Expr>>(
        *payload, [](const msgpack::object &o) { return parseSpanned<ast::Expr>(o, parseExpr); });
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Timeout") {
    ast::ExprTimeout e;
    e.expr = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "expr"), parseExpr));
    e.duration = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "duration"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Unsafe")
    return ast::Expr{ast::ExprUnsafe{parseBlock(*payload)}, {}};
  if (name == "Yield") {
    ast::ExprYield e;
    if (payload && !isNil(*payload)) {
      e.value =
          std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*payload, parseExpr));
    }
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Cooperate")
    return ast::Expr{ast::ExprCooperate{}, {}};
  if (name == "This")
    return ast::Expr{ast::ExprThis{}, {}};
"#
}

fn expr_projection_dispatcher() -> &'static str {
    r#"  if (name == "FieldAccess") {
    ast::ExprFieldAccess e;
    e.object = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "object"), parseExpr));
    e.field = getString(mapReq(*payload, "field"));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Index") {
    ast::ExprIndex e;
    e.object = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "object"), parseExpr));
    e.index = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "index"), parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Cast") {
    ast::ExprCast e;
    e.expr = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "expr"), parseExpr));
    e.ty = parseSpanned<ast::TypeExpr>(mapReq(*payload, "ty"), parseTypeExpr);
    return ast::Expr{std::move(e), {}};
  }
  if (name == "PostfixTry") {
    ast::ExprPostfixTry e;
    e.inner =
        std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*payload, parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Range") {
    ast::ExprRange e;
    const auto *s = mapGet(*payload, "start");
    if (s && !isNil(*s)) {
      e.start = std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*s, parseExpr));
    }
    const auto *en = mapGet(*payload, "end");
    if (en && !isNil(*en)) {
      e.end = std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*en, parseExpr));
    }
    e.inclusive = getBool(mapReq(*payload, "inclusive"));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "Await") {
    ast::ExprAwait e;
    e.inner =
        std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*payload, parseExpr));
    return ast::Expr{std::move(e), {}};
  }
  if (name == "ScopeLaunch")
    return ast::Expr{ast::ExprScopeLaunch{parseBlock(*payload)}, {}};
  if (name == "ScopeSpawn")
    return ast::Expr{ast::ExprScopeSpawn{parseBlock(*payload)}, {}};
  if (name == "ScopeCancel")
    return ast::Expr{ast::ExprScopeCancel{}, {}};

  if (name == "RegexLiteral")
    return ast::Expr{ast::ExprRegexLiteral{getString(*payload)}, {}};
  if (name == "ByteStringLiteral")
    return ast::Expr{
        ast::ExprByteStringLiteral{parseVec<uint8_t>(
            *payload, [](const msgpack::object &o) { return static_cast<uint8_t>(getInt(o)); })},
        {}};
  if (name == "ByteArrayLiteral")
    return ast::Expr{
        ast::ExprByteArrayLiteral{parseVec<uint8_t>(
            *payload, [](const msgpack::object &o) { return static_cast<uint8_t>(getInt(o)); })},
        {}};
"#
}

fn expr_dispatch_footer() -> &'static str {
    r#"  fail("unknown Expr variant: " + name);
}"#
}

fn expr_parser_parts() -> [&'static str; 8] {
    [
        expr_dispatch_header(),
        expr_scalar_and_collection_dispatcher(),
        expr_control_flow_dispatcher(),
        expr_actor_dispatcher(),
        expr_call_dispatcher(),
        expr_async_dispatcher(),
        expr_projection_dispatcher(),
        expr_dispatch_footer(),
    ]
}

pub fn expr_parser() -> &'static str {
    static PARSER: OnceLock<String> = OnceLock::new();
    PARSER.get_or_init(|| stitch(&expr_parser_parts())).as_str()
}

/// Hard-coded parser lifted from the working C++ reader.
pub fn else_block_parser() -> &'static str {
    r#"static ast::ElseBlock parseElseBlock(const msgpack::object &obj) {
  ast::ElseBlock eb;
  eb.is_if = getBool(mapReq(obj, "is_if"));
  const auto *ifs = mapGet(obj, "if_stmt");
  if (ifs && !isNil(*ifs)) {
    eb.if_stmt = parseSpannedPtr<ast::Stmt>(*ifs, parseStmt);
  }
  const auto *blk = mapGet(obj, "block");
  if (blk && !isNil(*blk))
    eb.block = parseBlock(*blk);
  return eb;
}"#
}

/// Hard-coded parser lifted from the working C++ reader.
fn stmt_dispatch_header() -> &'static str {
    r"static ast::Stmt parseStmt(const msgpack::object &obj) {
  auto [name, payload] = getEnumVariant(obj);

"
}

fn stmt_binding_dispatcher() -> &'static str {
    r#"  if (name == "Let") {
    ast::StmtLet s;
    s.pattern = parseSpanned<ast::Pattern>(mapReq(*payload, "pattern"), parsePattern);
    const auto *ty = mapGet(*payload, "ty");
    if (ty && !isNil(*ty))
      s.ty = parseSpanned<ast::TypeExpr>(*ty, parseTypeExpr);
    const auto *val = mapGet(*payload, "value");
    if (val && !isNil(*val))
      s.value = parseSpanned<ast::Expr>(*val, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Var") {
    ast::StmtVar s;
    s.name = getString(mapReq(*payload, "name"));
    const auto *ty = mapGet(*payload, "ty");
    if (ty && !isNil(*ty))
      s.ty = parseSpanned<ast::TypeExpr>(*ty, parseTypeExpr);
    const auto *val = mapGet(*payload, "value");
    if (val && !isNil(*val))
      s.value = parseSpanned<ast::Expr>(*val, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Assign") {
    ast::StmtAssign s;
    s.target = parseSpanned<ast::Expr>(mapReq(*payload, "target"), parseExpr);
    const auto *op = mapGet(*payload, "op");
    if (op && !isNil(*op))
      s.op = parseCompoundAssignOp(*op);
    s.value = parseSpanned<ast::Expr>(mapReq(*payload, "value"), parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
"#
}

fn stmt_branching_dispatcher() -> &'static str {
    r#"  if (name == "If") {
    ast::StmtIf s;
    s.condition = parseSpanned<ast::Expr>(mapReq(*payload, "condition"), parseExpr);
    s.then_block = parseBlock(mapReq(*payload, "then_block"));
    const auto *eb = mapGet(*payload, "else_block");
    if (eb && !isNil(*eb))
      s.else_block = parseElseBlock(*eb);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "IfLet") {
    ast::StmtIfLet s;
    s.pattern = parseSpanned<ast::Pattern>(mapReq(*payload, "pattern"), parsePattern);
    s.expr = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "expr"), parseExpr));
    s.body = parseBlock(mapReq(*payload, "body"));
    const auto *eb = mapGet(*payload, "else_body");
    if (eb && !isNil(*eb))
      s.else_body = parseBlock(*eb);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Match") {
    ast::StmtMatch s;
    s.scrutinee = parseSpanned<ast::Expr>(mapReq(*payload, "scrutinee"), parseExpr);
    s.arms = parseVec<ast::MatchArm>(mapReq(*payload, "arms"), parseMatchArm);
    return ast::Stmt{std::move(s), {}};
  }
"#
}

fn stmt_loop_dispatcher() -> &'static str {
    r#"  if (name == "Loop") {
    ast::StmtLoop s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    s.body = parseBlock(mapReq(*payload, "body"));
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "For") {
    ast::StmtFor s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    s.is_await = getBool(mapReq(*payload, "is_await"));
    s.pattern = parseSpanned<ast::Pattern>(mapReq(*payload, "pattern"), parsePattern);
    s.iterable = parseSpanned<ast::Expr>(mapReq(*payload, "iterable"), parseExpr);
    s.body = parseBlock(mapReq(*payload, "body"));
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "While") {
    ast::StmtWhile s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    s.condition = parseSpanned<ast::Expr>(mapReq(*payload, "condition"), parseExpr);
    s.body = parseBlock(mapReq(*payload, "body"));
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "WhileLet") {
    ast::StmtWhileLet s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    s.pattern = parseSpanned<ast::Pattern>(mapReq(*payload, "pattern"), parsePattern);
    s.expr = std::make_unique<ast::Spanned<ast::Expr>>(
        parseSpanned<ast::Expr>(mapReq(*payload, "expr"), parseExpr));
    s.body = parseBlock(mapReq(*payload, "body"));
    return ast::Stmt{std::move(s), {}};
  }
"#
}

fn stmt_terminator_dispatcher() -> &'static str {
    r#"  if (name == "Break") {
    ast::StmtBreak s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    const auto *val = mapGet(*payload, "value");
    if (val && !isNil(*val))
      s.value = parseSpanned<ast::Expr>(*val, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Continue") {
    ast::StmtContinue s;
    const auto *lbl = mapGet(*payload, "label");
    if (lbl && !isNil(*lbl))
      s.label = getString(*lbl);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Return") {
    ast::StmtReturn s;
    if (payload && !isNil(*payload))
      s.value = parseSpanned<ast::Expr>(*payload, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Defer") {
    ast::StmtDefer s;
    s.expr =
        std::make_unique<ast::Spanned<ast::Expr>>(parseSpanned<ast::Expr>(*payload, parseExpr));
    return ast::Stmt{std::move(s), {}};
  }
  if (name == "Expression") {
    ast::StmtExpression s;
    s.expr = parseSpanned<ast::Expr>(*payload, parseExpr);
    return ast::Stmt{std::move(s), {}};
  }
"#
}

fn stmt_dispatch_footer() -> &'static str {
    r#"  fail("unknown Stmt variant: " + name);
}"#
}

fn stmt_parser_parts() -> [&'static str; 6] {
    [
        stmt_dispatch_header(),
        stmt_binding_dispatcher(),
        stmt_branching_dispatcher(),
        stmt_loop_dispatcher(),
        stmt_terminator_dispatcher(),
        stmt_dispatch_footer(),
    ]
}

pub fn stmt_parser() -> &'static str {
    static PARSER: OnceLock<String> = OnceLock::new();
    PARSER.get_or_init(|| stitch(&stmt_parser_parts())).as_str()
}

/// Hard-coded parser lifted from the working C++ reader.
pub fn machine_transition_parser() -> &'static str {
    r#"static ast::MachineTransition parseMachineTransition(const msgpack::object &obj) {
  ast::MachineTransition mt;
  mt.event_name = getString(mapReq(obj, "event_name"));
  mt.source_state = getString(mapReq(obj, "source_state"));
  mt.target_state = getString(mapReq(obj, "target_state"));
  const auto *g = mapGet(obj, "guard");
  if (g && !isNil(*g))
    mt.guard = parseSpannedPtr<ast::Expr>(*g, parseExpr);
  mt.body = parseSpanned<ast::Expr>(mapReq(obj, "body"), parseExpr);
  return mt;
}"#
}
/// Hard-coded parser lifted from the working C++ reader.
pub fn child_spec_parser() -> &'static str {
    r#"static ast::ChildSpec parseChildSpec(const msgpack::object &obj) {
  ast::ChildSpec cs;
  cs.name = getString(mapReq(obj, "name"));
  cs.actor_type = getString(mapReq(obj, "actor_type"));
  cs.args = parseVec<ast::Spanned<ast::Expr>>(mapReq(obj, "args"), [](const msgpack::object &o) {
    return parseSpanned<ast::Expr>(o, parseExpr);
  });
  const auto *r = mapGet(obj, "restart");
  if (r && !isNil(*r)) {
    auto s = getString(*r);
    if (s == "Permanent")
      cs.restart = ast::RestartPolicy::Permanent;
    else if (s == "Transient")
      cs.restart = ast::RestartPolicy::Transient;
    else if (s == "Temporary")
      cs.restart = ast::RestartPolicy::Temporary;
    else
      fail("unknown RestartPolicy: " + s);
  }
  return cs;
}"#
}

/// Hard-coded parser for `ReceiveFnDecl` because the C++ AST stores a derived
/// periodic interval instead of the raw `attributes` vector.
pub fn receive_fn_decl_parser() -> &'static str {
    r#"static ast::ReceiveFnDecl parseReceiveFnDecl(const msgpack::object &obj) {
  ast::ReceiveFnDecl rf;
  rf.is_generator = getBool(mapReq(obj, "is_generator"));
  rf.is_pure = getBool(mapReq(obj, "is_pure"));
  rf.name = getString(mapReq(obj, "name"));
  const auto *tp = mapGet(obj, "type_params");
  if (tp && !isNil(*tp))
    rf.type_params = parseVec<ast::TypeParam>(*tp, parseTypeParam);
  rf.params = parseVec<ast::Param>(mapReq(obj, "params"), parseParam);
  const auto *rt = mapGet(obj, "return_type");
  if (rt && !isNil(*rt))
    rf.return_type = parseSpanned<ast::TypeExpr>(*rt, parseTypeExpr);
  const auto *wc = mapGet(obj, "where_clause");
  if (wc && !isNil(*wc))
    rf.where_clause = parseWhereClause(*wc);
  rf.body = parseBlock(mapReq(obj, "body"));
  rf.span = parseSpan(mapReq(obj, "span"));

  const auto *attrs = mapGet(obj, "attributes");
  if (attrs && !isNil(*attrs) && attrs->type == msgpack::type::ARRAY) {
    for (uint32_t i = 0; i < attrs->via.array.size; ++i) {
      const auto &attrObj = attrs->via.array.ptr[i];
      auto attrName = getString(mapReq(attrObj, "name"));
      if (attrName != "every")
        continue;
      const auto *argsArr = mapGet(attrObj, "args");
      if (!argsArr || isNil(*argsArr) || argsArr->type != msgpack::type::ARRAY ||
          argsArr->via.array.size != 1)
        continue;

      const auto &argObj = argsArr->via.array.ptr[0];
      auto [variantName, payload] = getEnumVariant(argObj);
      if (variantName == "Duration" && payload)
        rf.periodic_interval_ns = getInt(*payload);
    }
  }

  return rf;
}"#
}

/// Public API functions at the end of the file.
pub fn public_api() -> &'static str {
    r"ast::Program parseMsgpackAST(const uint8_t *data, size_t size) {
  msgpack::object_handle oh = msgpack::unpack(reinterpret_cast<const char *>(data), size);
  return parseProgram(oh.get());
}"
}

/// File header (includes and namespace).
pub fn file_header() -> &'static str {
    r#"//===- msgpack_reader_gen.cpp - GENERATED - Deserialize msgpack AST --------===//
//
// AUTO-GENERATED by hew-astgen from hew-parser/src/ast.rs
// DO NOT EDIT - changes will be overwritten.
//
//===----------------------------------------------------------------------===//
// clang-format off

#include "hew/msgpack_reader.h"

#include <msgpack.hpp>

#include <cassert>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

namespace hew {"#
}

/// Helper functions preamble (shared utilities used by all parsers).
#[expect(
    clippy::too_many_lines,
    reason = "shared generated C++ helper preamble keeps fail-closed wire helpers together"
)]
pub fn helpers_preamble() -> String {
    let mut out = String::from(
        r#"// ── Error helper ────────────────────────────────────────────────────────────

[[noreturn]] static void fail(const std::string &msg) {
  throw std::runtime_error("msgpack AST parse error: " + msg);
}

// ── msgpack object helpers ──────────────────────────────────────────────────

/// Get a string from a msgpack object.
static std::string getString(const msgpack::object &obj) {
  if (obj.type != msgpack::type::STR)
    fail("expected string, got type " + std::to_string(obj.type));
  return std::string(obj.via.str.ptr, obj.via.str.size);
}

/// Get integer from msgpack object.
static int64_t getInt(const msgpack::object &obj) {
  if (obj.type == msgpack::type::POSITIVE_INTEGER) {
    if (obj.via.u64 > static_cast<uint64_t>(INT64_MAX))
      fail("unsigned value " + std::to_string(obj.via.u64) + " overflows int64_t");
    return static_cast<int64_t>(obj.via.u64);
  }
  if (obj.type == msgpack::type::NEGATIVE_INTEGER)
    return obj.via.i64;
  fail("expected integer, got type " + std::to_string(obj.type));
}

/// Get unsigned integer from msgpack object.
static uint64_t getUint(const msgpack::object &obj) {
  if (obj.type == msgpack::type::POSITIVE_INTEGER)
    return obj.via.u64;
  if (obj.type == msgpack::type::NEGATIVE_INTEGER)
    fail("negative value " + std::to_string(obj.via.i64) + " cannot be converted to uint64_t");
  fail("expected unsigned integer, got type " + std::to_string(obj.type));
}

/// Get an exact uint32_t from msgpack object.
static uint32_t getUint32(const msgpack::object &obj, std::string_view context) {
  const auto value = getUint(obj);
  if (value > static_cast<uint64_t>(UINT32_MAX))
    fail(std::string(context) + " value " + std::to_string(value) + " overflows uint32_t");
  return static_cast<uint32_t>(value);
}

/// Get float from msgpack object.
static double getFloat(const msgpack::object &obj) {
  if (obj.type == msgpack::type::FLOAT32 || obj.type == msgpack::type::FLOAT64)
    return obj.via.f64;
  if (obj.type == msgpack::type::POSITIVE_INTEGER)
    return static_cast<double>(obj.via.u64);
  if (obj.type == msgpack::type::NEGATIVE_INTEGER)
    return static_cast<double>(obj.via.i64);
  fail("expected float, got type " + std::to_string(obj.type));
}

/// Get bool from msgpack object.
static bool getBool(const msgpack::object &obj) {
  if (obj.type == msgpack::type::BOOLEAN)
    return obj.via.boolean;
  fail("expected bool, got type " + std::to_string(obj.type));
}

/// Parse a Visibility enum from a msgpack object (string variant name).
static ast::Visibility parseVisibility(const msgpack::object &obj) {
  auto s = getString(obj);
  if (s == "Pub")
    return ast::Visibility::Pub;
  if (s == "PubPackage")
    return ast::Visibility::PubPackage;
  if (s == "PubSuper")
    return ast::Visibility::PubSuper;
  if (s == "Private")
    return ast::Visibility::Private;
  fail("unknown Visibility variant: " + s);
}

/// Check if msgpack object is nil.
static bool isNil(const msgpack::object &obj) {
  return obj.type == msgpack::type::NIL;
}

/// Interpret a msgpack object as a map and find a key.
/// Returns nullptr if not found.
static const msgpack::object *mapGet(const msgpack::object &obj, std::string_view key) {
  if (obj.type != msgpack::type::MAP)
    fail("expected map, got type " + std::to_string(obj.type));
  for (uint32_t i = 0; i < obj.via.map.size; ++i) {
    const auto &kv = obj.via.map.ptr[i];
    if (kv.key.type == msgpack::type::STR &&
        std::string_view(kv.key.via.str.ptr, kv.key.via.str.size) == key)
      return &kv.val;
  }
  return nullptr;
}

/// Interpret a msgpack object as a map and get a required key.
static const msgpack::object &mapReq(const msgpack::object &obj, std::string_view key) {
  const auto *v = mapGet(obj, key);
  if (!v)
    fail("missing required key: " + std::string(key));
  return *v;
}

/// Get a map as an array of key-value pairs.
static const msgpack::object_kv *mapEntries(const msgpack::object &obj, uint32_t &size) {
  if (obj.type != msgpack::type::MAP)
    fail("expected map, got type " + std::to_string(obj.type));
  size = obj.via.map.size;
  return obj.via.map.ptr;
}

/// Get an array from a msgpack object.
static const msgpack::object *arrayData(const msgpack::object &obj, uint32_t &size) {
  if (obj.type != msgpack::type::ARRAY)
    fail("expected array, got type " + std::to_string(obj.type));
  size = obj.via.array.size;
  return obj.via.array.ptr;
}

/// Get the variant name from an externally-tagged enum.
/// Returns the variant name and a pointer to the payload.
/// For unit variants (encoded as bare string), payload is nullptr.
static std::pair<std::string, const msgpack::object *> getEnumVariant(const msgpack::object &obj) {
  // Unit variant: encoded as a bare string
  if (obj.type == msgpack::type::STR)
    return {getString(obj), nullptr};
  // Map with single entry: {"VariantName": payload}
  if (obj.type == msgpack::type::MAP && obj.via.map.size == 1) {
    const auto &kv = obj.via.map.ptr[0];
    return {getString(kv.key), &kv.val};
  }
  fail("expected enum variant (string or single-entry map), got type " + std::to_string(obj.type));
}

/// Exact schema version this reader understands. The embedded Rust→C++
/// boundary is internal to the current `hew` binary, so missing or
/// mismatched versions are rejected rather than carrying compatibility
/// fallbacks for older payloads.
constexpr uint32_t CURRENT_SCHEMA_VERSION = "#,
    );
    out.push_str(&CURRENT_SCHEMA_VERSION.to_string());
    out.push(';');
    out
}

/// Span/Spanned/Optional/Vec template helpers.
pub fn template_helpers() -> &'static str {
    r#"// ── Span / Spanned ──────────────────────────────────────────────────────────

static ast::Span parseSpan(const msgpack::object &obj) {
  // Rust Range<usize> serializes as {"start": N, "end": N}
  return {getUint(mapReq(obj, "start")), getUint(mapReq(obj, "end"))};
}

template <typename T, typename ParseFn>
static ast::Spanned<T> parseSpanned(const msgpack::object &obj, ParseFn parseFn) {
  // Spanned<T> = (T, Span) → [T_value, span_map]
  uint32_t size;
  const auto *arr = arrayData(obj, size);
  if (size != 2)
    fail("Spanned tuple should have 2 elements");
  ast::Spanned<T> result{parseFn(arr[0]), parseSpan(arr[1])};
  // Copy span into inner type for codegen convenience (Expr/Stmt have span field)
  if constexpr (std::is_same_v<T, ast::Expr> || std::is_same_v<T, ast::Stmt>) {
    result.value.span = result.span;
  }
  return result;
}

/// Parse a Spanned<T> and wrap in unique_ptr (for forward-declared T).
template <typename T, typename ParseFn>
static std::unique_ptr<ast::Spanned<T>> parseSpannedPtr(const msgpack::object &obj,
                                                        ParseFn parseFn) {
  return std::make_unique<ast::Spanned<T>>(parseSpanned<T>(obj, parseFn));
}

// ── Optional helpers ────────────────────────────────────────────────────────

template <typename T, typename ParseFn>
static std::optional<T> parseOptional(const msgpack::object &obj, ParseFn parseFn) {
  if (isNil(obj))
    return std::nullopt;
  return parseFn(obj);
}

template <typename T, typename ParseFn>
static std::vector<T> parseVec(const msgpack::object &obj, ParseFn parseFn) {
  uint32_t size;
  const auto *arr = arrayData(obj, size);
  std::vector<T> result;
  result.reserve(size);
  for (uint32_t i = 0; i < size; ++i)
    result.push_back(parseFn(arr[i]));
  return result;
}

template <typename T, typename ParseFn>
static std::vector<T> parseOptVec(const msgpack::object &obj, ParseFn parseFn) {
  if (isNil(obj))
    return {};
  return parseVec<T>(obj, parseFn);
}

/// Parse a vector of unique_ptr<T> from a msgpack array.
template <typename T, typename ParseFn>
static std::vector<std::unique_ptr<T>> parseVecPtr(const msgpack::object &obj, ParseFn parseFn) {
  uint32_t size;
  const auto *arr = arrayData(obj, size);
  std::vector<std::unique_ptr<T>> result;
  result.reserve(size);
  for (uint32_t i = 0; i < size; ++i)
    result.push_back(std::make_unique<T>(parseFn(arr[i])));
  return result;
}"#
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;

    use crate::model::TypeDef;
    use crate::parse;

    use super::*;

    // ── Verifying special-case parsers contain their function signatures ────
    //
    // These catch accidental truncation or corruption of the hard-coded C++
    // parser strings. Each test asserts the presence of the function signature
    // and a distinctive internal detail, confirming the body is intact.

    #[test]
    fn literal_parser_has_all_variant_branches() {
        let src = literal_parser();
        assert!(src.contains("parseLiteral(const msgpack::object &obj)"));
        // All six literal kinds must be present
        for variant in &["Integer", "Float", "String", "Bool", "Char", "Duration"] {
            assert!(
                src.contains(&format!("name == \"{variant}\"")),
                "Missing Literal variant: {variant}"
            );
        }
    }

    fn serialized_variant_names(type_name: &str) -> Vec<String> {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let ast_path = manifest_dir.join("../hew-parser/src/ast.rs");
        let ast_source = fs::read_to_string(&ast_path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {e}", ast_path.display()));
        let types = parse::extract_types(&ast_path.display().to_string(), &ast_source)
            .expect("hew-parser/src/ast.rs should stay parseable");
        let tagged = types
            .iter()
            .find_map(|ty| match ty {
                TypeDef::TaggedEnum(tagged) if tagged.name == type_name => Some(tagged),
                _ => None,
            })
            .unwrap_or_else(|| panic!("Missing tagged enum {type_name}"));
        tagged
            .variants
            .iter()
            .map(|variant| variant.name().to_string())
            .collect()
    }

    fn assert_variant_coverage(type_name: &str, coverage: &[(&str, VariantDisposition)]) {
        let mut actual = serialized_variant_names(type_name);
        actual.sort();

        let mut covered: Vec<String> = coverage
            .iter()
            .map(|(name, _)| (*name).to_string())
            .collect();
        covered.sort();
        assert_eq!(
            covered, actual,
            "{type_name} parser coverage must match serialized variants"
        );
        assert!(
            coverage.iter().all(|(_, disposition)| {
                matches!(
                    disposition,
                    VariantDisposition::Parsed | VariantDisposition::Rejected
                )
            }),
            "{type_name} parser coverage must classify every variant"
        );
    }

    #[test]
    fn expr_parser_covers_every_serialized_variant() {
        assert_variant_coverage("Expr", EXPR_VARIANT_COVERAGE);
    }

    #[test]
    fn stmt_parser_covers_every_serialized_variant() {
        assert_variant_coverage("Stmt", STMT_VARIANT_COVERAGE);
    }

    #[test]
    fn expr_parser_stitches_topical_dispatchers() {
        let src = expr_parser();
        assert!(src.contains("name == \"Binary\""));
        assert!(src.contains("name == \"ScopeCancel\""));
        assert!(src.contains("unknown Expr variant"));
    }

    #[test]
    fn stmt_parser_stitches_topical_dispatchers() {
        let src = stmt_parser();
        assert!(src.contains("name == \"Let\""));
        assert!(src.contains("name == \"Expression\""));
        assert!(src.contains("unknown Stmt variant"));
    }

    #[test]
    fn expr_type_entry_parser_reads_start_end_ty() {
        let src = expr_type_entry_parser();
        assert!(src.contains("parseExprTypeEntry("));
        assert!(src.contains("entry.start"));
        assert!(src.contains("entry.end"));
        assert!(src.contains("entry.ty"));
    }

    #[test]
    fn method_call_receiver_kind_entry_parser_reads_shape() {
        let src = method_call_receiver_kind_entry_parser();
        assert!(src.contains("parseMethodCallReceiverKindEntry("));
        assert!(src.contains("entry.start"));
        assert!(src.contains("entry.end"));
        assert!(src.contains("named_type_instance"));
        assert!(src.contains("handle_instance"));
        assert!(src.contains("trait_object"));
        assert!(src.contains("stream_instance"));
        assert!(src.contains("type_name"));
        assert!(src.contains("trait_name"));
        assert!(src.contains("element_kind"));
    }

    #[test]
    fn assign_target_kind_data_parser_handles_all_variants() {
        let src = assign_target_kind_data_parser();
        assert!(src.contains("local_var"));
        assert!(src.contains("actor_field"));
        assert!(src.contains("field_access"));
        assert!(src.contains("index"));
    }

    #[test]
    fn assign_target_kind_entry_parser_reads_kind_tag() {
        let src = assign_target_kind_entry_parser();
        assert!(src.contains("parseAssignTargetKindEntry("));
        assert!(src.contains("entry.start"));
        assert!(src.contains("entry.end"));
        assert!(src.contains("mapReq(obj, \"kind\")"));
        assert!(src.contains("parseAssignTargetKindData"));
    }

    #[test]
    fn lowering_fact_entry_parser_reads_all_fields() {
        let src = lowering_fact_entry_parser();
        assert!(src.contains("parseLoweringKind("));
        assert!(src.contains("hash_set"));
        assert!(src.contains("parseHashSetElementType("));
        assert!(src.contains("i64"));
        assert!(src.contains("u64"));
        assert!(src.contains("str"));
        assert!(src.contains("parseHashSetAbi("));
        assert!(src.contains("int64"));
        assert!(src.contains("string"));
        assert!(src.contains("parseDropKind("));
        assert!(src.contains("hash_set_free"));
        assert!(src.contains("parseLoweringFactEntry("));
        assert!(src.contains("entry.element_type"));
        assert!(src.contains("entry.abi_variant"));
        assert!(src.contains("entry.drop_kind"));
    }

    #[test]
    fn module_graph_parser_iterates_modules_map() {
        let src = module_graph_parser();
        assert!(src.contains("parseModuleGraph("));
        assert!(src.contains("mg.root"));
        assert!(src.contains("mg.topo_order"));
        assert!(src.contains("modulesObj.type == msgpack::type::ARRAY"));
        assert!(src.contains("modulesObj.type == msgpack::type::MAP"));
        assert!(
            src.contains("mg.modules.emplace"),
            "Should iterate and emplace module entries"
        );
    }

    #[test]
    fn module_id_parser_accepts_string_keys() {
        let src = module_id_parser();
        assert!(src.contains("obj.type == msgpack::type::STR"));
        assert!(src.contains("s == \"(root)\""));
        assert!(src.contains("s.find(\"::\", pos)"));
        assert!(src.contains("mapReq(obj, \"path\")"));
    }

    #[test]
    fn program_parser_handles_optional_fields() {
        let src = program_parser();
        assert!(src.contains("parseProgram("));
        // Required fields
        assert!(src.contains("mapReq(obj, \"schema_version\")"));
        assert!(src.contains("prog.items"));
        // Required metadata fields stay strict at the embedded boundary.
        assert!(src.contains("mapReq(obj, \"expr_types\")"));
        assert!(src.contains("mapReq(obj, \"method_call_receiver_kinds\")"));
        assert!(src.contains("mapReq(obj, \"assign_target_kinds\")"));
        assert!(src.contains("mapReq(obj, \"lowering_facts\")"));
        assert!(src.contains("mapReq(obj, \"handle_types\")"));
        assert!(src.contains("mapReq(obj, \"handle_bearing_structs\")"));
        assert!(src.contains("mapReq(obj, \"handle_type_repr\")"));
        assert!(src.contains("prog.lowering_facts"));
        // Optional fields checked with mapGet
        assert!(src.contains("mapGet(obj, \"module_doc\")"));
        assert!(src.contains("mapGet(obj, \"module_graph\")"));
        // drop_funcs: optional (absent for programs with no stdlib handle drops)
        assert!(
            src.contains("mapGet(obj, \"drop_funcs\")"),
            "program parser must read drop_funcs"
        );
        assert!(
            src.contains("prog.drop_funcs[ty] = func"),
            "program parser must populate drop_funcs map"
        );
    }

    #[test]
    fn type_decl_parser_handles_method_storage() {
        let src = type_decl_parser();
        assert!(src.contains("parseTypeDecl("));
        assert!(
            src.contains("td.method_storage"),
            "TypeDecl parser must manage method_storage ownership"
        );
        // All three TypeBodyItem variants
        assert!(src.contains("name == \"Field\""));
        assert!(src.contains("name == \"Variant\""));
        assert!(src.contains("name == \"Method\""));
        assert!(src.contains("td.wire = parseWireMetadata(*w);"));
        assert!(src.contains("td.is_indirect = getBool(*ind);"));
    }

    #[test]
    fn helpers_preamble_uses_explicit_visibility_encoding() {
        let src = helpers_preamble();
        assert!(src.contains("static ast::Visibility parseVisibility"));
        assert!(
            !src.contains("Backward compatibility: old format used a bool."),
            "Visibility decoding should not retain legacy bool fallbacks"
        );
    }

    #[test]
    fn helpers_preamble_tracks_serialize_schema_version() {
        assert_eq!(
            CURRENT_SCHEMA_VERSION,
            hew_serialize::msgpack::SCHEMA_VERSION
        );
        let src = helpers_preamble();
        assert!(src.contains(&format!(
            "constexpr uint32_t CURRENT_SCHEMA_VERSION = {CURRENT_SCHEMA_VERSION};"
        )));
    }

    #[test]
    fn helpers_preamble_emits_u32_overflow_guard() {
        let src = helpers_preamble();
        assert!(src.contains("static uint32_t getUint32"));
        assert!(src.contains("overflows uint32_t"));
    }

    #[test]
    fn program_parser_uses_checked_schema_version_parse() {
        let src = program_parser();
        assert!(src.contains("getUint32(mapReq(obj, \"schema_version\"), \"schema_version\")"));
    }

    #[test]
    fn receive_fn_decl_parser_extracts_every_duration() {
        let src = receive_fn_decl_parser();
        assert!(src.contains("if (attrName != \"every\")"));
        assert!(src.contains("variantName == \"Duration\""));
        assert!(src.contains("rf.periodic_interval_ns = getInt(*payload);"));
    }

    #[test]
    fn attribute_arg_parser_rejects_duration_variant() {
        let src = attribute_arg_parser();
        assert!(src.contains("variant == \"Positional\""));
        assert!(src.contains("variant == \"KeyValue\""));
        assert!(!src.contains("AttributeArgDuration"));
    }

    #[test]
    fn public_api_exposes_both_parse_entry_points() {
        let src = public_api();
        assert!(
            src.contains("parseMsgpackAST("),
            "Missing msgpack entry point"
        );
        assert!(
            !src.contains("parseJsonAST("),
            "Unexpected JSON entry point"
        );
    }

    #[test]
    fn file_header_sets_namespace_and_includes() {
        let src = file_header();
        assert!(src.contains("namespace hew {"));
        assert!(src.contains("#include \"hew/msgpack_reader.h\""));
        assert!(src.contains("#include <msgpack.hpp>"));
        assert!(!src.contains("nlohmann/json.hpp"));
    }

    #[test]
    fn helpers_preamble_provides_core_utilities() {
        let src = helpers_preamble();
        // Every auto-generated parser depends on these helpers
        for fn_name in &[
            "getString",
            "getInt",
            "getUint",
            "getFloat",
            "getBool",
            "isNil",
            "mapGet",
            "mapReq",
            "getEnumVariant",
        ] {
            assert!(src.contains(fn_name), "Missing helper function: {fn_name}");
        }
    }

    #[test]
    fn template_helpers_provide_parse_templates() {
        let src = template_helpers();
        for template in &[
            "parseSpan",
            "parseSpanned",
            "parseSpannedPtr",
            "parseOptional",
            "parseVec",
            "parseOptVec",
            "parseVecPtr",
        ] {
            assert!(
                src.contains(template),
                "Missing template helper: {template}"
            );
        }
    }
}
