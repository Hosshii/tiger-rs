/// https://github.com/WebAssembly/spec/tree/master/interpreter/#s-expression-syntax

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name(pub String);

impl Name {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

pub enum ValType {
    Num(NumType),
}

pub struct FuncType {
    pub params: Vec<Param>,
    pub result: Vec<WasmResult>,
}

impl FuncType {
    pub fn new(params: Vec<Param>, result: Vec<WasmResult>) -> Self {
        Self { params, result }
    }
}

pub enum BinOp {
    Add,
    Sub,
}

pub enum Expr {
    Op(Operator),
    OpExpr(Operator, Vec<Expr>),
}

pub enum Instruction {
    Expr(Expr),
    Op(Operator),
}

pub enum Operator {
    GlobalGet(Index),
    Bin(NumType, BinOp),
    Const(NumType, i64),
}

pub enum Index {
    Index(u32),
    Name(Name),
}

pub struct Func {
    pub name: Option<Name>,
    pub type_idx: Index,
    pub locals: Vec<Local>,
    pub instr: Vec<Instruction>,
}

impl Func {
    pub fn new(
        name: Option<Name>,
        type_idx: Index,
        locals: Vec<Local>,
        instr: Vec<Instruction>,
    ) -> Self {
        Self {
            name,
            type_idx,
            locals,
            instr,
        }
    }
}

pub struct Param {
    pub type_: ValType,
    pub name: String,
}

pub struct WasmResult(pub ValType);

impl WasmResult {
    pub fn new(val_type: ValType) -> Self {
        Self(val_type)
    }
}

pub struct Local {
    pub type_: ValType,
    pub name: String,
}

pub struct FuncTypeDef {
    pub name: Option<Name>,
    pub params: Vec<Param>,
    pub result: Vec<WasmResult>,
}

pub struct Import {
    pub str1: String,
    pub str2: String,
    pub kind: ImportKind,
}

pub enum ImportKind {
    Func(Option<Name>, FuncType),
}

pub struct Export {
    name: Name,
    kind: ExportKind,
}

pub enum ExportKind {
    Func(Index),
}

pub struct Module {
    pub types: Vec<FuncTypeDef>,
    pub func: Vec<Func>,
    pub import: Vec<Import>,
    pub export: Vec<Export>,
}

impl Module {
    pub fn empty() -> Self {
        Self {
            types: vec![],
            func: vec![],
            import: vec![],
            export: vec![],
        }
    }

    pub fn builder() -> ModuleBuilder {
        ModuleBuilder::new()
    }
}

pub struct ModuleBuilder {
    module: Module,
}

impl ModuleBuilder {
    pub fn new() -> Self {
        Self {
            module: Module::empty(),
        }
    }

    pub fn build(self) -> Module {
        self.module
    }

    pub fn add_func(mut self, func: Func) -> Self {
        self.module.func.push(func);
        self
    }
    pub fn add_type(mut self, type_: FuncTypeDef) -> Self {
        self.module.types.push(type_);
        self
    }
}
