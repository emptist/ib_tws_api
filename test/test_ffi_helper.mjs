// Helper to test what Gleam passes to FFI layer
export function send_bytes_test(data) {
    console.log("\n[FFI HELPER] send_bytes_test called");
    console.log("[FFI HELPER] data type:", typeof data);
    console.log("[FFI HELPER] data constructor:", data.constructor.name);
    console.log("[FFI HELPER] data:", data);

    // Try to inspect the structure
    if (data && typeof data === 'object') {
        console.log("[FFI HELPER] Object keys:", Object.keys(data));
        console.log("[FFI HELPER] Object entries:", Object.entries(data));
    }

    // Try to convert to Buffer using the FIXED method
    let buffer;
    try {
        buffer = Buffer.from(data.rawBuffer);
        console.log("[FFI HELPER] Buffer created successfully");
        console.log("[FFI HELPER] Buffer length:", buffer.length);
        console.log("[FFI HELPER] Buffer hex:", buffer.toString('hex'));
        console.log("[FFI HELPER] Buffer as string:", buffer.toString());
        return "SUCCESS: Buffer created with hex " + buffer.toString('hex');
    } catch (e) {
        console.error("[FFI HELPER] Buffer.from failed:", e.message);
        return "ERROR: " + e.message;
    }
}