import * as net from 'net';

/// Create a TCP socket connection to IB TWS API
export function connect(config) {
    return new Promise((resolve, reject) => {
        const socket = new net.Socket();
        const host = config.host;
        const port = config.port;

        // Set timeout for connection (10 seconds)
        socket.setTimeout(10000, () => {
            socket.destroy();
            reject({ type: 'Timeout' });
        });

        // Connect to the server
        socket.connect(port, host, () => {
            // Connection successful
            socket.setTimeout(0); // Disable timeout
            resolve({
                type: 'Ok',
                value: {
                    socket: socket,
                    host: host,
                    port: port
                }
            });
        });

        // Handle connection errors
        socket.on('error', (err) => {
            reject({
                type: 'Error',
                value: { type: 'ConnectionFailed', message: err.message }
            });
        });
    });
}

/// Send data through the connection
export function send(conn, data) {
    return new Promise((resolve, reject) => {
        if (!conn || !conn.socket) {
            reject({ type: 'Error', value: { type: 'SocketError', message: 'Connection is closed' } });
            return;
        }

        try {
            conn.socket.write(data, (err) => {
                if (err) {
                    reject({ type: 'Error', value: { type: 'SocketError', message: err.message } });
                } else {
                    resolve({ type: 'Ok', value: undefined });
                }
            });
        } catch (err) {
            reject({ type: 'Error', value: { type: 'SocketError', message: err.message } });
        }
    });
}

/// Receive data from the connection (returns all available data)
export function receive(conn) {
    return new Promise((resolve, reject) => {
        if (!conn || !conn.socket) {
            reject({ type: 'Error', value: { type: 'SocketError', message: 'Connection is closed' } });
            return;
        }

        // Set a timeout for receiving data (5 seconds)
        const timeout = setTimeout(() => {
            socket.removeAllListeners('data');
            reject({ type: 'Error', value: { type: 'Timeout' } });
        }, 5000);

        const socket = conn.socket;

        // Listen for data
        socket.once('data', (data) => {
            clearTimeout(timeout);
            resolve({ type: 'Ok', value: data.toString('utf-8') });
        });

        // Handle socket errors
        socket.once('error', (err) => {
            clearTimeout(timeout);
            reject({ type: 'Error', value: { type: 'SocketError', message: err.message } });
        });
    });
}

/// Close the connection
export function close(conn) {
    return new Promise((resolve, reject) => {
        if (!conn || !conn.socket) {
            resolve({ type: 'Ok', value: undefined });
            return;
        }

        try {
            conn.socket.end(() => {
                resolve({ type: 'Ok', value: undefined });
            });
        } catch (err) {
            reject({ type: 'Error', value: { type: 'SocketError', message: err.message } });
        }
    });
}