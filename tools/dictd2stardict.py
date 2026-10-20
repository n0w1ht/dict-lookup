"""Convert a dictd dictionary (.index + .dict.dz) to plain-text StarDict."""
import gzip, os, struct, sys

B64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
def b64(s):
    n = 0
    for c in s:
        n = n * 64 + B64.index(c)
    return n

# Old FreeDict English-Hungarian data writes ő/ű as the Latin-1 ô/û.
HU_FIX = str.maketrans("ôûÔÛ", "őűŐŰ")

def main(index, dictdz, outdir, name, bookname, fix=""):
    data = gzip.open(dictdz).read()
    entries = {}
    order = []
    for line in open(index, encoding="utf-8", errors="replace"):
        parts = line.rstrip("\n").split("\t")
        if len(parts) < 3 or parts[0].startswith("00-database") or parts[0].startswith("00database"):
            continue
        word, off, ln = parts[0], b64(parts[1]), b64(parts[2])
        text = data[off:off + ln].decode("utf-8", "replace").strip("\n")
        if fix == "hu-accents":
            text = text.translate(HU_FIX)
        key = word
        if key not in entries:
            entries[key] = []
            order.append(key)
        if text not in entries[key]:
            entries[key].append(text)
    os.makedirs(outdir, exist_ok=True)
    words = sorted(order, key=lambda w: (w.encode().lower(), w.encode()))
    idx = bytearray()
    with open(os.path.join(outdir, name + ".dict"), "wb") as d:
        off = 0
        written = {}  # identical bodies are stored once and shared
        for w in words:
            body = "\n\n".join(entries[w]).encode("utf-8")
            if body not in written:
                d.write(body)
                written[body] = off
                off += len(body)
            idx += w.encode("utf-8") + b"\0" + struct.pack(">II", written[body], len(body))
    with open(os.path.join(outdir, name + ".idx"), "wb") as f:
        f.write(idx)
    with open(os.path.join(outdir, name + ".ifo"), "w", encoding="utf-8", newline="\n") as f:
        f.write("StarDict's dict ifo file\nversion=2.4.2\n"
                f"wordcount={len(words)}\nidxfilesize={len(idx)}\n"
                f"bookname={bookname}\nsametypesequence=m\n")
    print(name, len(words), "words")

if __name__ == "__main__":
    main(*sys.argv[1:])
