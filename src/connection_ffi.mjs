export function get_timestamp() {
    return Date.now().toString();
}

export function set_timeout(milliseconds, callback) {
    setTimeout(callback, milliseconds);
}

export function sleep(milliseconds) {
    return new Promise(resolve => setTimeout(resolve, milliseconds));
}

export function send_bytes(socket, data) {
    // Debug: Check what type we received
    console.log(`[send_bytes] Input type: ${typeof data}`);
    console.log(`[send_bytes] Input constructor: ${data?.constructor?.name}`);
    console.log(`[send_bytes] Input value:`, data);

    // Convert Gleam BitArray to Node.js Buffer
    // BitArray has a rawBuffer property containing the actual Uint8Array
    const buffer = Buffer.from(data.rawBuffer);

    // Log for debugging
    console.log(`[send_bytes] Buffer length: ${buffer.length}`);
    console.log(`[send_bytes] Buffer as hex: ${buffer.toString('hex')}`);
    console.log(`[send_bytes] Bytes as numbers: [${Array.from(buffer).join(', ')}]`);

    return socket.write(buffer);
}