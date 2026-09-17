import os
import stat
import subprocess
import sys


def symlink_dir(src, dst):
    src = os.path.abspath(src)
    dst = os.path.abspath(dst)
    if not os.path.isdir(src):
        raise ValueError(f"Junction source is not a directory: {src}")

    if os.path.lexists(dst):
        info = os.lstat(dst)
        if getattr(info, "st_reparse_tag", None) != stat.IO_REPARSE_TAG_MOUNT_POINT:
            raise ValueError(f"Refusing to replace a non-junction destination: {dst}")
        if os.path.exists(dst) and os.path.samefile(src, dst):
            return
        # Remove only the junction itself, including when its target is missing.
        os.rmdir(dst)

    os.makedirs(os.path.dirname(dst), exist_ok=True)
    subprocess.run(
        [os.environ.get("COMSPEC", "cmd.exe"), "/d", "/c", "mklink", "/J", dst, src],
        check=True,
    )


if __name__ == "__main__":
    try:
        symlink_dir(*sys.argv[1:])
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"Cannot create directory junction: {error}", file=sys.stderr)
        sys.exit(1)
