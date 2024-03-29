use crate::semant::hir::Operator as HirOperator;

/// https://github.com/WebAssembly/spec/tree/master/interpreter/#s-expression-syntax

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(pub String);

impl Name {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl<T: Into<String>> From<T> for Name {
    fn from(name: T) -> Self {
        Self(name.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum ValType {
    Num(NumType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockType(pub TypeUse<FuncType>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub params: Vec<Param>,
    pub result: Vec<WasmResult>,
}

impl FuncType {
    pub fn new(params: Vec<Param>, result: Vec<WasmResult>) -> Self {
        Self { params, result }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    DivSigned,
    Eq,
    Ne,
    LessThanSigned,
    LessOrEqualSigned,
    GreaterThanSigned,
    GreaterOrEqualSigned,
}

impl TryFrom<HirOperator> for BinOp {
    type Error = ();

    fn try_from(op: HirOperator) -> Result<Self, Self::Error> {
        match op {
            HirOperator::Plus => Ok(Self::Add),
            HirOperator::Minus => Ok(Self::Sub),
            HirOperator::Mul => Ok(Self::Mul),
            HirOperator::Div => Ok(Self::DivSigned),
            HirOperator::Eq => Ok(Self::Eq),
            HirOperator::Neq => Ok(Self::Ne),
            HirOperator::Lt => Ok(Self::LessThanSigned),
            HirOperator::Le => Ok(Self::LessOrEqualSigned),
            HirOperator::Gt => Ok(Self::GreaterThanSigned),
            HirOperator::Ge => Ok(Self::GreaterOrEqualSigned),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Sign {
    Singed,
    Unsigned,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CvtOp {
    Wrap,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TestOp {
    Eqz,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Comment(String, Box<Expr>),
    Op(Operator),
    OpExpr(Operator, Vec<Expr>),
    Block(Option<Name>, BlockType, Vec<Instruction>),
    If(
        Option<Name>,
        BlockType,
        Vec<Expr>,
        Vec<Instruction>,
        Option<Vec<Instruction>>,
    ),
    Loop(Option<Name>, BlockType, Vec<Instruction>),
}

impl Expr {
    pub fn add_comment(self, comment: impl Into<String>) -> Self {
        Self::Comment(comment.into(), Box::new(self))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    Expr(Expr),
    Op(Operator),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
    Br(Index),
    BrIf(Index),
    Call(Index),
    GlobalGet(Index),
    GlobalSet(Index),
    LocalGet(Index),
    LocalSet(Index),
    Store(NumType), // NumType.store
    Load(NumType),
    Bin(NumType, BinOp),
    Test(NumType, TestOp),
    Const(NumType, i64),
    Convert(NumType, NumType, CvtOp, Option<Sign>),
    Nop,
    Drop,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Index {
    Index(IndexNumber),
    Name(Name),
}

impl From<IndexNumber> for Index {
    fn from(idx: u32) -> Self {
        Self::Index(idx)
    }
}

pub type IndexNumber = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]

pub enum TypeUse<T> {
    Index(Index),
    Inline(T),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InlineFuncExport {
    pub name: Name,
}

impl InlineFuncExport {
    pub fn new(name: Name) -> Self {
        Self { name }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub type_: ValType,
    pub name: Option<Name>,
}

impl Param {
    pub fn new(type_: ValType, name: Option<Name>) -> Self {
        Self { type_, name }
    }
}

impl From<ValType> for Param {
    fn from(val_type: ValType) -> Self {
        Self {
            type_: val_type,
            name: None,
        }
    }
}

impl From<Param> for ValType {
    fn from(param: Param) -> Self {
        param.type_
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
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

impl From<WasmResult> for ValType {
    fn from(result: WasmResult) -> Self {
        result.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Local {
    pub type_: ValType,
    pub name: Option<Name>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncTypeDef {
    pub name: Option<Name>,
    pub ty: FuncType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub kind: ImportKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportKind {
    Func(Option<Index>, TypeUse<FuncType>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Export {
    pub name: Name,
    pub kind: ExportKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExportKind {
    Func(Index),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Global {
    pub name: Option<Name>,
    pub ty: GlobalType,
    pub init: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobalType {
    pub ty: ValType,
    pub m: Mut,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Mut {
    Const,
    Var,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Memory {
    pub name: Option<Name>,
    pub ty: MemoryType,
}

pub type MemoryType = Limits;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DataString(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Data {
    pub name: Option<Name>,
    pub offset: Expr,
    pub init: DataString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub types: Vec<FuncTypeDef>,
    pub funcs: Vec<Func>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub globals: Vec<Global>,
    pub memories: Vec<Memory>,
    pub data: Vec<Data>,
}

impl Module {
    pub fn empty() -> Self {
        Self {
            types: vec![],
            funcs: vec![],
            imports: vec![],
            exports: vec![],
            globals: vec![],
            memories: vec![],
            data: vec![],
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

    pub fn add_funcs(mut self, func: Vec<Func>) -> Self {
        self.module.funcs.extend(func);
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

    pub fn add_memory(mut self, memory: Memory) -> Self {
        self.module.memories.push(memory);
        self
    }

    pub fn add_imports(mut self, imports: Vec<Import>) -> Self {
        self.module.imports.extend(imports);
        self
    }
}
