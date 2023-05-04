#!/bin/bash
head -n -1 test/build/$1.hex > mem.vmh
python3 arrange_mem.py

#!/bin/bash
# # head -n -1 test/build/$1.hex > mem.vmh
# if [[ $OSTYPE == 'darwin'* ]]; then
# 	echo 'macOS'
# 	sed '$ d' test/build/$1.hex > mem.vmh
# else
# 	head -n -1 test/build/$1.hex > mem.vmh
# fi

