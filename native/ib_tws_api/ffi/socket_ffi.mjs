import { Socket } from 'net';

export function connect(host, port, callback) {
  console.log(`[Node.js] Connecting to ${host}:${port}`);

  const socket = new Socket();

  socket.setTimeout(10000, () => {
    console.log('[Node.js] Connection timeout triggered');
    socket.destroy();
    callback({ success: false, error: new Error('Connection timeout') });
  });

  socket.connect(port, host, () => {
    console.log(`[Node.js] Connected successfully to ${host}:${port}`);
    callback({ success: true, value: socket });
  });

  socket.on('error', (err) => {
    console.log(`[Node.js] Error event: ${err.message}`);
    socket.destroy();
    callback({ success: false, error: err });
  });

  socket.on('close', (hadError) => {
    console.log(`[Node.js] Close event, hadError=${hadError}`);
  });
}

export function send(socket, data, callback) {
  if (!socket || socket.destroyed) {
    callback({ success: false, error: new Error('Socket not connected') });
    return;
  }

  const buffer = Buffer.from(data);
  console.log(`[Node.js] Sending ${buffer.length} bytes`);

  socket.write(buffer, (err) => {
    if (err) {
      callback({ success: false, error: err });
    } else {
      callback({ success: true, value: null });
    }
  });
}

export function recv(socket, length, timeout, callback) {
  if (!socket || socket.destroyed) {
    callback({ success: false, error: new Error('Socket not connected') });
    return;
  }

  console.log(`[Node.js] Receiving up to ${length} bytes with timeout ${timeout}ms`);

  socket.setTimeout(timeout, () => {
    socket.removeAllListeners('data');
    callback({ success: false, error: new Error('Receive timeout') });
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
    console.log(`[Node.js] Received ${chunk.length} bytes`);
    chunks.push(chunk);
    totalLength += chunk.length;

    if (totalLength >= length) {
      resolved = true;
      cleanup();
      const buffer = Buffer.concat(chunks);
      callback({ success: true, value: new Uint8Array(buffer) });
    }
  });

  socket.on('error', (err) => {
    cleanup();
    callback({ success: false, error: err });
  });

  socket.on('close', () => {
    cleanup();
    if (!resolved && chunks.length > 0) {
      resolved = true;
      const buffer = Buffer.concat(chunks);
      callback({ success: true, value: new Uint8Array(buffer) });
    } else if (!resolved) {
      callback({ success: false, error: new Error('Socket closed') });
    }
  });
}

export function close(socket, callback) {
  if (!socket || socket.destroyed) {
    callback({ success: true, value: null });
    return;
  }

  console.log('[Node.js] Closing socket');

  socket.end(() => {
    socket.destroy();
    callback({ success: true, value: null });
  });

  socket.on('error', (err) => {
    socket.destroy();
    callback({ success: false, error: err });
  });
}
