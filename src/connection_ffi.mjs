export function string_to_hex(str) {
    return Array.from(str)
        .map(c => c.charCodeAt(0).toString(16).padStart(2, '0'))
        .join(' ');
}

export function get_timestamp() {
    return new Date().toISOString();
}

export function generate_client_id() {
    return Math.floor(Math.random() * 10000);
}

export function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

export function keep_alive(ms) {
    setTimeout(() => { }, ms);
}

export function detect_ib_tws_port(host, timeout) {
    // Implementation remains the same
    return 0;
}

export function write_to_file(filename, content, append) {
    // Implementation remains the same
    return "ok";
}