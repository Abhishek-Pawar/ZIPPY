cd examples ;
mkdir compressed ;
cd ../;

{ ./zippy -c-lzw  examples/Facebook.html examples/compressed/fb.lzw;
  ./zippy -c-lzw  examples/50kb.txt examples/compressed/50kb.lzw ;
  ./zippy -c-lzw  examples/100kb.txt examples/compressed/100kb.lzw;
  ./zippy -c-lzw  examples/556kb.csv examples/compressed/556kb.lzw;}

{ ./zippy -c-huff  examples/Facebook.html examples/compressed/fb.huff;
  ./zippy -c-huff examples/50kb.txt examples/compressed/50kb.huff ;
  ./zippy -c-huff  examples/100kb.txt examples/compressed/100kb.huff;
  ./zippy -c-huff  examples/556kb.csv examples/compressed/556kb.huff;}

cd examples;
mkdir uncompressed;
cd ../;

{ ./zippy -d-lzw examples/compressed/fb.lzw examples/uncompressed/FB_lzw.html;
  ./zippy -d-lzw examples/compressed/50kb.lzw examples/uncompressed/50kb_lzw.txt;
  ./zippy -d-lzw examples/compressed/100kb.lzw examples/uncompressed/100kb_lzw.txt;
  ./zippy -d-lzw examples/compressed/556kb.lzw examples/uncompressed/556kb_lzw.csv; }

{ ./zippy -d-huff examples/compressed/fb.huff examples/uncompressed/FB_huff.html;
  ./zippy -d-huff examples/compressed/50kb.huff examples/uncompressed/50kb_huff.txt;
  ./zippy -d-huff examples/compressed/100kb.huff examples/uncompressed/100kb_huff.txt;
  ./zippy -d-huff examples/compressed/556kb.huff examples/uncompressed/556kb_huff.csv; }

{ diff examples/Facebook.html examples/uncompressed/FB_lzw.html;
  diff examples/50kb.txt examples/uncompressed/50kb_lzw.txt;
  diff examples/100kb.txt examples/uncompressed/100kb_lzw.txt;
  diff examples/556kb.csv examples/uncompressed/556kb_lzw.csv; }

{ diff examples/Facebook.html examples/uncompressed/FB_huff.html;
  diff examples/50kb.txt examples/uncompressed/50kb_huff.txt ;
  diff examples/100kb.txt examples/uncompressed/100kb_huff.txt;
  diff examples/556kb.csv examples/uncompressed/556kb_huff.csv; }
