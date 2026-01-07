/**
 * Foreign Function Interface (FFI) for Node.js integration
 * Provides JavaScript-specific functionality for the Gleam IB TWS API
 */

/**
 * Get current timestamp as string
 * @returns {string} Current timestamp in milliseconds
 */
export function get_timestamp() {
    return Date.now().toString();
}

/**
 * Sleep for specified milliseconds (non-blocking)
 * @param {number} milliseconds - Milliseconds to sleep
 * @returns {Promise<void>} Promise that resolves after the delay
 */
export function sleep(milliseconds) {
    return new Promise((resolve) => setTimeout(resolve, milliseconds));
}

/**
 * Convert Gleam BitArray to Node.js Buffer and send to socket
 * @param {import('net').Socket} socket - Node.js socket instance
 * @param {import('gleam').BitArray} data - BitArray data to send
 * @returns {boolean} True if successful, False otherwise
 */
export function send_bytes(socket, data) {
    try {
        // Get the underlying Uint8Array from the BitArray
        const bytes = data.rawBuffer;

        // Create Buffer from the bytes
        const buffer = Buffer.from(bytes);

        // Write buffer to socket
        const result = socket.write(buffer);
        return result;
    } catch (error) {
        console.error('[send_bytes] Error:', error);
        return false;
    }
}

/**
 * Generate a random client ID based on current timestamp
 * Ensures unique client ID per connection to avoid conflicts
 * @returns {number} Time-based random client ID
 */
export function generate_client_id() {
    // Use timestamp with random component for uniqueness
    const timestamp = Date.now();
    const random = Math.floor(Math.random() * 10000);
    return (timestamp % 1000000) + random;
}

/**
 * Filter control characters from a string
 * Removes ASCII control characters (0x00-0x1f) except space (0x20)
 * @param {string} str - Input string
 * @returns {string} String with control characters removed
 */
export function filter_control_characters(str) {
    return str.replace(/[\x00-\x1F]/g, '');
}