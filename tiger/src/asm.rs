use std::fmt::Display;

use crate::common::{Label as CommonLabel, Temp as CommonTemp};

type Register = String;
type Label = CommonLabel;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Temp(CommonTemp);

impl Temp {
    pub fn new() -> Self {
        Self(CommonTemp::new())
    }

    pub fn new_with(num: u32) -> Self {
        Self(CommonTemp::new_with(num))
    }
}

impl Display for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl From<CommonTemp> for Temp {
    fn from(tmp: CommonTemp) -> Self {
        Self(tmp)
    }
}

impl From<&CommonTemp> for Temp {
    fn from(tmp: &CommonTemp) -> Self {
        Self(*tmp)
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Operand {
        assembly: String,
        dst: Vec<Temp>,
        src: Vec<Temp>,
        jump: Option<Vec<Label>>,
    },
    Label {
        assembly: String,
        label: CommonLabel,
    },
    Move {
        assembly: String,
        dst: Temp,
        src: Temp,
    },
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
