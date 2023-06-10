use crate::semant::hir::Operator as HirOperator;

/// https://github.com/WebAssembly/spec/tree/master/interpreter/#s-expression-syntax

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(pub String);

impl Name {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValType {
    Num(NumType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockType(pub TypeUse<FuncType>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub params: Vec<Param>,
    pub result: Vec<WasmResult>,
}

impl FuncType {
    pub fn new(params: Vec<Param>, result: Vec<WasmResult>) -> Self {
        Self { params, result }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
}

impl TryFrom<HirOperator> for BinOp {
    type Error = ();

    fn try_from(op: HirOperator) -> Result<Self, Self::Error> {
        match op {
            HirOperator::Plus => Ok(Self::Add),
            HirOperator::Minus => Ok(Self::Sub),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Op(Operator),
    OpExpr(Operator, Vec<Expr>),
    Block(Option<Name>, BlockType, Vec<Instruction>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Expr(Expr),
    Op(Operator),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    GlobalGet(Index),
    LocalGet(Index),
    LocalSet(Index),
    Store(NumType), // NumType.store
    Load(NumType),
    Bin(NumType, BinOp),
    Const(NumType, i64),
    Nop,
    Drop,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Index {
    Index(IndexNumber),
    Name(Name),
}

type IndexNumber = u32;

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum TypeUse<T> {
    Index(Index),
    Inline(T),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlineFuncExport {
    pub name: Name,
}

impl InlineFuncExport {
    pub fn new(name: Name) -> Self {
        Self { name }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub name: Option<Name>,
    pub ty: TypeUse<FuncType>,
    pub export: Option<InlineFuncExport>,
    pub locals: Vec<Local>,
    pub instr: Vec<Instruction>,
}

impl Func {
    pub fn new(
        name: Option<Name>,
        ty: TypeUse<FuncType>,
        export: Option<InlineFuncExport>,
        locals: Vec<Local>,
        instr: Vec<Instruction>,
    ) -> Self {
        Self {
            name,
            ty,
            export,
            locals,
            instr,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub type_: ValType,
    pub name: Option<Name>,
}

impl Param {
    pub fn new(type_: ValType, name: Option<Name>) -> Self {
        Self { type_, name }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmResult(pub ValType);

impl WasmResult {
    pub fn new(val_type: ValType) -> Self {
        Self(val_type)
    }
}

impl From<ValType> for WasmResult {
    fn from(val_type: ValType) -> Self {
        Self(val_type)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Local {
    pub type_: ValType,
    pub name: Option<Name>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncTypeDef {
    pub name: Option<Name>,
    pub ty: FuncType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub str1: String,
    pub str2: String,
    pub kind: ImportKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportKind {
    Func(Option<Name>, FuncType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Export {
    pub name: Name,
    pub kind: ExportKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportKind {
    Func(Index),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Global {
    pub name: Option<Name>,
    pub ty: GlobalType,
    pub init: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalType {
    pub ty: ValType,
    pub m: Mut,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mut {
    Const,
    Var,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub types: Vec<FuncTypeDef>,
    pub funcs: Vec<Func>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub globals: Vec<Global>,
}

impl Module {
    pub fn empty() -> Self {
        Self {
            types: vec![],
            funcs: vec![],
            imports: vec![],
            exports: vec![],
            globals: vec![],
        }
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
        self.module.funcs.push(func);
        self
    }

    pub fn _add_type(mut self, type_: FuncTypeDef) -> Self {
        self.module.types.push(type_);
        self
    }

    pub fn add_globals(mut self, globals: Vec<Global>) -> Self {
        self.module.globals.extend(globals);
        self
    }
}
