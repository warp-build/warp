use std::io::IsTerminal;

/// Style a short status label when stderr is an interactive terminal.
pub fn status_tag(label: &str, success: bool) -> String {
    style_status_tag(label, success, std::io::stderr().is_terminal())
}

/// Style a short status label when stdout is an interactive terminal.
pub fn stdout_status_tag(label: &str, success: bool) -> String {
    style_status_tag(label, success, std::io::stdout().is_terminal())
}

fn style_status_tag(label: &str, success: bool, terminal: bool) -> String {
    if !terminal || std::env::var_os("NO_COLOR").is_some() {
        return label.to_string();
    }

    let color = if success { 32 } else { 31 };
    format!("\x1b[{color};1m{label}\x1b[0m")
}
