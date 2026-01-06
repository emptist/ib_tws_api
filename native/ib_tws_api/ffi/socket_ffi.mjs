import { Socket } from 'net';

export function connect(host, port) {
  return new Promise((resolve, reject) => {
    console.log(`[Socket FFI] Connecting to ${host}:${port}`);

    const socket = new Socket();

    socket.setTimeout(10000, () => {
      socket.destroy();
      reject(new Error('Connection timeout'));
    });

    socket.connect(port, host, () => {
      console.log(`[Socket FFI] Connected successfully`);
      resolve(socket);
    });

    socket.on('error', (err) => {
      console.log(`[Socket FFI] Connection error: ${err.message}`);
      reject(err);
    });
  });
}

export function send(socket, data) {
  return new Promise((resolve, reject) => {
    if (!socket || socket.destroyed) {
      reject(new Error('Socket not connected'));
      return;
    }

    const buffer = Buffer.from(data);
    console.log(`[Socket FFI] Sending ${buffer.length} bytes`);

    socket.write(buffer, (err) => {
      if (err) {
        reject(err);
      } else {
        resolve(null);
      }
    });
  });
}

export function recv(socket, length, timeout) {
  return new Promise((resolve, reject) => {
    if (!socket || socket.destroyed) {
      reject(new Error('Socket not connected'));
      return;
    }

    console.log(`[Socket FFI] Receiving up to ${length} bytes with timeout ${timeout}ms`);

    socket.setTimeout(timeout, () => {
      socket.removeAllListeners('data');
      reject(new Error('Receive timeout'));
    });

    const chunks = [];
    let totalLength = 0;
    let resolved = false;

    const cleanup = () => {
      if (!resolved) {
        socket.setTimeout(0);
        socket.removeAllListeners('data');
      }
    };

    socket.on('data', (chunk) => {
      console.log(`[Socket FFI] Received ${chunk.length} bytes`);
      chunks.push(chunk);
      totalLength += chunk.length;

      if (totalLength >= length) {
        resolved = true;
        cleanup();
        const buffer = Buffer.concat(chunks);
        resolve(new Uint8Array(buffer));
      }
    });

    socket.on('error', (err) => {
      cleanup();
      reject(err);
    });

    socket.on('close', () => {
      cleanup();
      if (!resolved && chunks.length > 0) {
        resolved = true;
        const buffer = Buffer.concat(chunks);
        resolve(new Uint8Array(buffer));
      } else if (!resolved) {
        reject(new Error('Socket closed'));
      }
    });
  });
}

export function close(socket) {
  return new Promise((resolve, reject) => {
    if (!socket || socket.destroyed) {
      resolve(null);
      return;
    }

    console.log('[Socket FFI] Closing socket');

    socket.end(() => {
      socket.destroy();
      resolve(null);
    });

    socket.on('error', (err) => {
      socket.destroy();
      reject(err);
    });
  });
}
