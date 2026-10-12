"""A slow loopback TLS peer with ephemeral local test certificates."""

from pathlib import Path
import socket
import subprocess
import tempfile
import ssl
import sys
import time

certificates = tempfile.TemporaryDirectory()
certificate_root = Path(certificates.name)


def openssl(*arguments):
    subprocess.run(
        ["openssl", *arguments],
        cwd=certificate_root,
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        timeout=15,
    )


openssl(
    "req",
    "-x509",
    "-newkey",
    "rsa:2048",
    "-nodes",
    "-days",
    "1",
    "-keyout",
    "ca.key",
    "-out",
    "ca.pem",
    "-subj",
    "/CN=Hew TLS test CA",
)
openssl(
    "req",
    "-newkey",
    "rsa:2048",
    "-nodes",
    "-keyout",
    "server.key",
    "-out",
    "server.csr",
    "-subj",
    "/CN=localhost",
)
(certificate_root / "server.ext").write_text(
    "subjectAltName=DNS:localhost\nbasicConstraints=CA:FALSE\n"
)
openssl(
    "x509",
    "-req",
    "-in",
    "server.csr",
    "-CA",
    "ca.pem",
    "-CAkey",
    "ca.key",
    "-CAcreateserial",
    "-out",
    "server.pem",
    "-days",
    "1",
    "-extfile",
    "server.ext",
)
Path("ca.pem").write_bytes((certificate_root / "ca.pem").read_bytes())
context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(
    certificate_root / "server.pem", certificate_root / "server.key"
)
with socket.socket() as listener:
    listener.bind(("127.0.0.1", 0))
    listener.listen()
    listener.settimeout(10)
    sys.stdout.buffer.write(listener.getsockname()[1].to_bytes(2, "big"))
    sys.stdout.buffer.flush()
    connection, _ = listener.accept()
    connection.settimeout(10)
    time.sleep(0.4)
    with context.wrap_socket(connection, server_side=True) as peer:
        assert peer.recv(5) == b"hello"
        time.sleep(0.5)
        peer.sendall(b"ready")
        time.sleep(0.5)
        tail = b""
        while not tail.endswith(b"DONE"):
            chunk = peer.recv(65536)
            assert chunk, "connection closed before retained write"
            tail = (tail + chunk)[-4:]
        peer.sendall(b"OK")
