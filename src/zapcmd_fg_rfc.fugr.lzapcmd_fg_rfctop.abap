FUNCTION-POOL ZAPCMD_FG_RFC.                "MESSAGE-ID ..

types: BEGIN OF ts_file,
            dirname(255) TYPE c, " name of directory. (possibly truncated.)
            name(255)    TYPE c, " name of entry. (possibly truncated.)
            type(10)     TYPE c,           " type of entry.
            len(8)       TYPE p,           " length in bytes.
            owner(8)     TYPE c,           " owner of the entry.
            mtime(6)     TYPE p, " last modification date,seconds since 1970
            mode(9)      TYPE c, " like "rwx-r-x--x": protection mode.
            usable(1)    TYPE c,
            subrc(4)     TYPE c,
            errno(3)     TYPE c,
            errmsg(40)   TYPE c,
            mod_date     TYPE d,
            mod_time(8)  TYPE c,           " hh:mm:ss
            seen(1)      TYPE c,
            changed(1)   TYPE c,
            rec_level    TYPE i,
            dir_flag     TYPE xflag,
          END OF ts_file.
 types:   tt_file type table of ts_file.
