import requests

files = {

    "https://www.cis.upenn.edu/~cis1940/spring13/extras/07-folds-monoids/Editor.hs" : "Editor.hs",
    "https://www.cis.upenn.edu/~cis1940/spring13/extras/07-folds-monoids/Buffer.hs" : "Buffer.hs",
    "https://www.cis.upenn.edu/~cis1940/spring13/extras/07-folds-monoids/Sized.hs"  : "Sized.hs",
    "https://www.cis.upenn.edu/~cis1940/spring13/extras/07-folds-monoids/StringBuffer.hs" : "StringBuffer.hs",
    "https://www.cis.upenn.edu/~cis1940/spring13/extras/07-folds-monoids/StringBufEditor.hs" : "StringBufEditor.hs",
    "https://www.cis.upenn.edu/~cis1940/spring13/extras/07-folds-monoids/carol.txt" : "carol.txt"
}

for path, fname in files.items():
    r = requests.get(path)
    with open(fname, 'w') as f:
        f.write(r.text)
