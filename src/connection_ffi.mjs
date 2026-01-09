import net from 'net';
import fs from 'fs';

// Check if a port is open on a host
export function check_port(host, port, timeout) {
    return new Promise((resolve) => {
        const socket = new net.Socket();
        let resolved = false;

        socket.setTimeout(timeout || 1000);

        socket.on('connect', () => {
            if (!resolved) {
                resolved = true;
                socket.destroy();
                resolve(true);
            }
        });

        socket.on('timeout', () => {
            if (!resolved) {
                resolved = true;
                socket.destroy();
                resolve(false);
            }
        });

        socket.on('error', () => {
            if (!resolved) {
                resolved = true;
                socket.destroy();
                resolve(false);
            }
        });

        socket.connect(port, host);
    });
}

// Get current timestamp as string for debugging
export function get_timestamp() {
    return Date.now().toString();
}

// Generate a random client ID based on current timestamp
export function generate_client_id() {
    return Date.now() % 1000000;
}

// Sleep for specified milliseconds using setTimeout
export function sleep(milliseconds) {
    return new Promise(resolve => setTimeout(resolve, milliseconds));
}

// Keep the Node.js process alive for specified milliseconds
export function keep_alive(milliseconds) {
    return new Promise(resolve => setTimeout(resolve, milliseconds));
}

// Detect which IB TWS port is available (7496 or 7497)
// Tests both ports and returns the first one that is available
// Returns 0 if neither port is available
export function detect_ib_tws_port(host, timeout) {
    const ports = [7497, 7496]; // Check paper trading first, then live trading

    function testPort(port) {
        return new Promise((resolve) => {
            const socket = new net.Socket();
            let resolved = false;

            socket.setTimeout(timeout || 1000);

            socket.on('connect', () => {
                if (!resolved) {
                    resolved = true;
                    socket.destroy();
                    resolve(port);
                }
            });

            socket.on('timeout', () => {
                if (!resolved) {
                    resolved = true;
                    socket.destroy();
                    resolve(0);
                }
            });

            socket.on('error', () => {
                if (!resolved) {
                    resolved = true;
                    socket.destroy();
                    resolve(0);
                }
            });

            socket.connect(port, host);
        });
    }

    // Test ports sequentially
    return testPort(ports[0]).then(result => {
        if (result !== 0) return result;
        return testPort(ports[1]);
    });
}

// Get current socket state
export function get_socket_current_state(socket) {
    return socket.state;
}

// Send raw bytes to a socket
export function send_bytes(socket, data) {
    try {
        // Convert BitArray (Uint8Array) to Buffer
        const buffer = Buffer.from(data);
        socket.write(buffer);
        return true;
    } catch (error) {
        console.error('Error sending bytes:', error);
        return false;
    }
}

// Get socket state
export function get_socket_state(socket) {
    return socket.state;
}

// Convert string to hex representation for debugging
export function string_to_hex(str) {
    let result = '';
    for (let i = 0; i < str.length; i++) {
        const hex = str.charCodeAt(i).toString(16).padStart(2, '0');
        result += hex + ' ';
    }
    return result.trim();
}

// Filter control characters from a string
// Removes ASCII control characters (0x00-0x1e) except space (0x20)
export function strip_leading_control_characters(str) {
    return str.replace(/[\x00-\x1f]+/g, '');
}

// Clean server handshake response
// Removes leading length bytes and control characters, preserves NULL byte
export function clean_server_response(data) {
    // Remove leading control characters (including length bytes)
    let cleaned = data.replace(/^[\x00-\x1f]+/, '');

    // Preserve the NULL byte separator between version and timestamp
    // But remove trailing control characters
    cleaned = cleaned.replace(/[\x01-\x1f]+$/, '');

    return cleaned;
}

// Convert float to string
export function float_to_string(f) {
    return f.toString();
}

// Write content to file for logging purposes
// Append parameter: true = append, false = overwrite
export function write_to_file(filename, content, append) {
    try {
        const options = append ? { flag: 'a' } : { flag: 'w' };
        fs.writeFileSync(filename, content + '\n', options);
        return { Ok: null };
    } catch (error) {
        return { Error: error.message };
    }
}