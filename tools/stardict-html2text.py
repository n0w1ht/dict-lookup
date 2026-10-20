"""Rewrite an HTML StarDict dictionary (sametypesequence=h) as plain text.

The .syn file is kept as is, since headword order does not change."""
import gzip, html, os, re, shutil, struct, sys

def to_text(body):
    out, n = [], 0
    for tok in re.split(r"(<[^>]+>)", body):
        t = tok.lower()
        if t == "<ol>":
            n = 0
        elif t == "<li>":
            n += 1
            out.append(f"\n  {n}. ")
        elif t in ("<br>", "<br/>", "<br />", "</ol>"):
            out.append("\n")
        elif t.startswith("<"):
            pass
        else:
            out.append(html.unescape(tok))
    text = re.sub(r"\n{3,}", "\n\n", "".join(out))
    return "\n".join(line.rstrip() for line in text.strip().split("\n"))

def main(src_ifo, outdir, name):
    base = src_ifo[:-4]
    ifo = dict(l.split("=", 1) for l in open(src_ifo, encoding="utf-8").read().splitlines()[1:] if "=" in l)
    assert ifo.get("sametypesequence") == "h"
    data = gzip.open(base + ".dict.dz").read()
    idx = open(base + ".idx", "rb").read()
    os.makedirs(outdir, exist_ok=True)
    new_idx = bytearray()
    pos, off, count = 0, 0, 0
    with open(os.path.join(outdir, name + ".dict"), "wb") as d:
        while pos < len(idx):
            end = idx.index(b"\0", pos)
            word = idx[pos:end]
            o, s = struct.unpack(">II", idx[end + 1:end + 9])
            pos = end + 9
            body = to_text(data[o:o + s].decode("utf-8")).encode("utf-8")
            d.write(body)
            new_idx += word + b"\0" + struct.pack(">II", off, len(body))
            off += len(body)
            count += 1
    open(os.path.join(outdir, name + ".idx"), "wb").write(new_idx)
    if os.path.exists(base + ".syn"):
        shutil.copy(base + ".syn", os.path.join(outdir, name + ".syn"))
    lines = ["StarDict's dict ifo file", "version=2.4.2",
             f"wordcount={count}", f"idxfilesize={len(new_idx)}",
             f"bookname={ifo['bookname']}", "sametypesequence=m"]
    if "synwordcount" in ifo:
        lines.append(f"synwordcount={ifo['synwordcount']}")
    with open(os.path.join(outdir, name + ".ifo"), "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(lines) + "\n")
    print(name, count, "words")

if __name__ == "__main__":
    main(*sys.argv[1:])
