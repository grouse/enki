import shutil
import sys
import os

src, dst = sys.argv[1], sys.argv[2]
os.makedirs(os.path.dirname(dst), exist_ok=True)
shutil.copy2(src, dst)
