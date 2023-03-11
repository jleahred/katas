import qbs

Project {
    CppApplication {
        consoleApplication: true
        cpp.cxxLanguageVersion: "c++20"
        //cpp.cxxLanguageVersion: "c++14"
        //cpp.cxxLanguageVersion: "c++17"
        //cpp.cxxLanguageVersion: "c++11"
        //cpp.cxxLanguageVersion: "c++2a"

        //cpp.includePaths: ["/home/maiquel/curro/mtk/", "/home/maiquel/curro/mtk/src", "/home/maiquel/curro/mtk/src/external"]

        cpp.libraryPaths: [
        //            "/home/maiquel/curro/mtk/lib",
        //            "/usr/local/lib64",
        ]
        cpp.staticLibraries: [
            "zmq",
            //            "mtkyaml",
            //            "qpidclient",
            //            "qpidmessaging",
            //            "qpidtypes",
            //            "qpidcommon",
            //            "pthread",
            //
            //  testing new ws
//            "boost_system", "ssl", "crypto",
            //  wt
//            "wthttp",  "wt"
        ]

//        cpp.cFlags: [
//        ]

        cpp.driverFlags: [
//            "-static",
//            "-static-libgcc",
//              "-DASIO_STANDALONE",
//              "-DUSE_STANDALONE_ASIO",
       ]

//        cpp.rpaths: ["$ORIGIN/lib"]

        cpp.linkerFlags: [
//            "-fuse-ld=mold"
            //"-stdlib=libstdc++ loopy.cpp -o loopy -static -lstdc++"
            //"-stdlib=libstdc++",
            //"-static-libstdc++",
            //"-static-libgcc",
            //"-lstdc++"
            //"-static",
            //"-nostdlib",
            //"-static-libgcc"
        ]
        cpp.cxxFlags: [
//            "-isystem", "/home/maiquel/curro/mtk/src/external",
            // "-std=c++14",
            "-Werror",
            "-Wall", "-W", "-Wundef", "-Wpointer-arith",   "-Wfloat-equal",  "-fexceptions",  "-Winit-self",  "-Wconversion",
            "-Wempty-body",   "-Wignored-qualifiers",  "-Wmissing-field-initializers",  "-Wsign-compare",  "-Wsign-conversion",  "-Wtype-limits",  "-Wuninitialized",  "-Wno-unused-result",
            "-Wnon-virtual-dtor", "-Wreorder",  "-Woverloaded-virtual",  "-Wsign-promo",  "-Winit-self",  "-Wignored-qualifiers",  "-Wmissing-include-dirs",
            "-Wswitch-enum",  "-Wswitch-default", "-Wshadow",  "-Wcast-qual",  "-Wwrite-strings"
        ]

        cpp.defines: [
//            "QPID_DECLARE_STATIC",
//            "QMAKE_CFLAGS_WARN_OFF",
        ]


        files: [
            "../src/**/*.cpp",
            "../src/**/*.h",
            "../src/**/*.hpp",
        ]

//        Group {     // Properties for the produced executable
//            fileTagsFilter: "application"
//            qbs.install: true
//        }
    }
}

