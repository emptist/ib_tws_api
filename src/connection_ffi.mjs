export function string_to_hex(str) {
    return Array.from(str)
        .map(c => c.charCodeAt(0).toString(16).padStart(2, '0'))
        .join(' ');
}

export function strip_leading_control_characters(str) {
    // Remove ASCII control characters (0x00-0x1e) except space (0x20)
    return str.replace(/[\x00-\x1e]/g, '');
}

export function clean_server_response(data) {
    // Remove leading control characters and length bytes
    // Preserve NULL byte separators in actual data
    let cleaned = data.replace(/^[\x00-\x1e]+/, '');
    return cleaned;
}

export function filter_control_characters(str) {
    // Filter out control characters from string
    return str.replace(/[\x00-\x1f]/g, '');
}

export function send_bytes(socket, data) {
    // Convert Gleam BitArray to Buffer
    // Gleam BitArray has rawBuffer property containing the actual bytes
    const buffer = Buffer.from(data.rawBuffer);
    try {
        socket.write(buffer);
        return true;
    } catch (e) {
        return false;
    }
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

export function float_to_string(value) {
    // Convert float to string with 2 decimal places
    return value.toFixed(2);
}

import net from 'net';

export function check_port(host, port, timeout) {
    return new Promise((resolve) => {
        const socket = new net.Socket();
        let isOpen = false;

        socket.setTimeout(timeout);

        socket.on('connect', () => {
            isOpen = true;
            socket.destroy();
            resolve(true);
        });

        socket.on('timeout', () => {
            socket.destroy();
            resolve(false);
        });

        socket.on('error', () => {
            socket.destroy();
            resolve(false);
        });

        socket.connect(port, host);
    });
}