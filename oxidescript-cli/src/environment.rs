use std::collections::HashMap;

use oxidescript::checker::{CheckContext, Variable};
use serde::Deserialize;

use crate::JavascriptRuntime;

#[derive(Clone, Debug, Default)]
pub enum JavascriptEnvironment {
    #[default]
    Browser,
    Bun,
    Node,
}

impl From<&str> for JavascriptEnvironment {
    fn from(value: &str) -> Self {
        match value {
            "browser" => JavascriptEnvironment::Browser,
            "bun" => JavascriptEnvironment::Bun,
            "node" => JavascriptEnvironment::Node,
            _ => Self::default(),
        }
    }
}

impl From<&JavascriptRuntime> for JavascriptEnvironment {
    fn from(value: &JavascriptRuntime) -> Self {
        match value {
            JavascriptRuntime::Bun => JavascriptEnvironment::Bun,
            JavascriptRuntime::Node => JavascriptEnvironment::Node,
        }
    }
}

#[derive(Deserialize)]
struct EnvironmentFile {
    variables: HashMap<String, Variable>,
}

impl JavascriptEnvironment {
    pub fn load(&self, ctx: &mut CheckContext) {
        let env_file = match self {
            JavascriptEnvironment::Browser => {
                include_str!("../environments/browser.toml")
            }
            JavascriptEnvironment::Bun => {
                include_str!("../environments/bun.toml")
            }
            JavascriptEnvironment::Node => {
                include_str!("../environments/node.toml")
            }
        };
        let parsed =
            toml::from_str::<EnvironmentFile>(env_file).expect("Failed to parse environment");
        ctx.scope.borrow_mut().variables.extend(parsed.variables);
    }
}
