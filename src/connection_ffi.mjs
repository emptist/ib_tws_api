/// JavaScript FFI for TCP socket operations
/// This file provides the actual TCP socket implementation using Node.js net module

import net from 'net';

/// Create a new TCP socket
export function create_socket() {
    return new Promise((resolve) => {
        const socket = new net.Socket();
        resolve(socket);
    });
}

/// Connect the socket to a host and port
export function connect(socket, host, port) {
    return new Promise((resolve, reject) => {
        socket.connect(port, host, () => {
            resolve();
        });

        socket.on('error', (err) => {
            reject(err);
        });
    });
}

/// Send data through the socket
export function send(socket, data) {
    return new Promise((resolve, reject) => {
        socket.write(data, (err) => {
            if (err) {
                reject(err);
            } else {
                resolve();
            }
        });
    });
}

/// Receive data from the socket
export function receive(socket) {
    return new Promise((resolve, reject) => {
        socket.on('data', (data) => {
            resolve(data.toString());
        });

        socket.on('error', (err) => {
            reject(err);
        });

        // Set a timeout to prevent hanging
        socket.setTimeout(5000, () => {
            socket.destroy();
            resolve('');
        });
    });
}

/// Close the socket
export function close(socket) {
    return new Promise((resolve) => {
        socket.end(() => {
            socket.destroy();
            resolve();
        });
    });
}