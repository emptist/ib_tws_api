/**
 * Foreign Function Interface (FFI) for Node.js integration
 * Provides JavaScript-specific functionality for the Gleam IB TWS API
 */
import { execSync } from 'child_process';

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

/**
 * Check if a TCP port is available (listening) on the local host
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