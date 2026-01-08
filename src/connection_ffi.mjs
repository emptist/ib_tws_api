/**
 * Foreign Function Interface (FFI) for Node.js integration
 * Provides JavaScript-specific functionality for Gleam IB TWS API
 */
import { execSync } from 'child_process';
import { appendFileSync, existsSync, mkdirSync } from 'fs';
import { dirname } from 'path';

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
 * Keep the Node.js process alive by preventing immediate exit
 * This is needed for event-driven code that waits for async callbacks
 * @param {number} milliseconds - How long to keep process alive
 * @returns {Promise<void>} Promise that resolves after the delay
 */
export function keep_alive(milliseconds) {
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

        // Create Buffer from bytes
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
 * Removes ASCII control characters (0x00-0x1f) except space (0x20) and unit separator (0x1f)
 * The unit separator (0x1f) is used by IB TWS to separate version from timestamp
 * @param {string} str - Input string
 * @returns {string} String with control characters removed (except 0x1f)
 */
export function filter_control_characters(str) {
    // Remove control characters 0x00-0x1e, but keep 0x1f (unit separator) and 0x20 (space)
    return str.replace(/[\x00-\x1E]/g, '');
}

/**
 * Strip leading control characters from a string
 * Only removes control characters from the START of the string
 * Keeps NULL bytes (0x00) and other control characters in the middle
 * @param {string} str - Input string
 * @returns {string} String with leading control characters removed
 */
export function strip_leading_control_characters(str) {
    // Remove only leading control characters 0x00-0x1F
    // Keep NULL (0x00) and other control characters in the middle of the string
    return str.replace(/^[\x00-\x1F]+/, '');
}

/**
 * Check if a TCP port is available (listening) on local host
 * Uses nc (netcat) command for synchronous port checking
 * @param {string} host - Hostname or IP address
 * @param {number} port - Port number to check
 * @param {number} timeout - Timeout in seconds (default: 1)
 * @returns {boolean} True if port is available, False otherwise
 */
export function check_port_available(host, port, timeout = 1) {
    try {
        // Use nc (netcat) to check if port is available
        // -z: Scan without sending any data
        // -w timeout: Timeout in seconds
        // Redirect stderr to stdout to capture all output
        const result = execSync(`nc -z -w ${timeout} ${host} ${port} 2>&1`, {
            encoding: 'utf8'
        });

        // If we get here, nc succeeded (exit code 0)
        // Check if output contains "succeeded" for confirmation
        return result.includes('succeeded');
    } catch (error) {
        // nc returns error if port is not available (exit code != 0)
        return false;
    }
}

/**
 * Detect which IB TWS port is available (7496 or 7497)
 * Tests both ports and returns the first one that is available
 * @param {string} host - Hostname or IP address (default: "127.0.0.1")
 * @param {number} timeout - Timeout in seconds for each port check (default: 1)
 * @returns {number} Port number if available, 0 if neither port is available
 */
export function detect_ib_tws_port(host = "127.0.0.1", timeout = 1) {
    console.log(`[Port Detection] Starting detection for ${host} with timeout ${timeout}s`);

    // Try port 7497 first (paper trading)
    const paper_port = 7497;
    console.log(`[Port Detection] Checking port ${paper_port} (Paper Trading)...`);
    const paper_available = check_port_available(host, paper_port, timeout);
    console.log(`[Port Detection] Port ${paper_port} available: ${paper_available}`);

    if (paper_available) {
        console.log(`[Port Detection] Port ${paper_port} (Paper Trading) is available`);
        console.log(`[Port Detection] Returning ${paper_port}`);
        return paper_port;
    }

    // Try port 7496 (live trading)
    const live_port = 7496;
    console.log(`[Port Detection] Checking port ${live_port} (Live Trading)...`);
    const live_available = check_port_available(host, live_port, timeout);
    console.log(`[Port Detection] Port ${live_port} available: ${live_available}`);

    if (live_available) {
        console.log(`[Port Detection] Port ${live_port} (Live Trading) is available`);
        console.log(`[Port Detection] Returning ${live_port}`);
        return live_port;
    }

    // Neither port is available
    console.log(`[Port Detection] Neither port 7496 nor 7497 is available on ${host}`);
    console.log(`[Port Detection] Returning 0`);
    return 0;
}

/**
 * Write content to file with safety checks
 * @param {string} filename - File path to write to
 * @param {string} content - Content to write
 * @param {boolean} append - Whether to append (true) or overwrite (false)
 * @returns {string} "Ok" on success, "Error" on failure
 */
export function write_to_file(filename, content, append) {
    try {
        // Ensure directory exists
        const directory = dirname(filename);
        if (!existsSync(directory)) {
            mkdirSync(directory, { recursive: true });
        }

        if (append) {
            appendFileSync(filename, content, 'utf8');
        } else {
            // For overwrite, we would use writeFileSync
            // But since gleam doesn't have overwrite support in the API yet
            // we'll implement append-only for now
            appendFileSync(filename, content, 'utf8');
        }

        return "Ok";
    } catch (error) {
        console.error('[write_to_file] Error:', error);
        return "Error";
    }
}

/**
 * Clean server handshake response
 * Removes leading length bytes and other control characters
 * Returns cleaned string with NULL bytes preserved
 * @param {string} data - Server response string
 * @returns {string} Cleaned string ready for parsing
 */
export function clean_server_response(data) {
    try {
        console.log('[clean_server_response] Raw data length:', data.length);

        // Skip first 4 bytes (message length) and parse the rest
        let messageData = data.length > 4 ? data.substring(4) : data;

        // Keep NULL bytes and printable characters, remove other control chars
        let filtered = '';
        for (let i = 0; i < messageData.length; i++) {
            let char = messageData[i];
            let code = char.charCodeAt(0);
            // Keep NULL separator (0x00) and printable characters (0x20-0x7e)
            if (code === 0x00 || (code >= 0x20 && code <= 0x7e)) {
                filtered += char;
            }
        }

        console.log('[clean_server_response] Filtered data hex:', filtered.split('').map(c => c.charCodeAt(0).toString(16).padStart(2, '0')).join(' '));

        // Trim whitespace from ends (but not NULL bytes in middle)
        let trimmed = filtered.replace(/^[\s]+|[\s]+$/g, '');

        console.log('[clean_server_response] Cleaned response:', trimmed);

        return trimmed;
    } catch (error) {
        console.error('[clean_server_response] Error:', error);
        return data;
    }
}

/**
 * Simple file append function for logging
 * @param {string} filename - File path to append to
 * @param {string} content - Content to append
 * @returns {string} "Ok" on success, "Error" on failure
 */
export function append_to_file(filename, content) {
    return write_to_file(filename, content, true);
}