solution "proto-gamelang"
  configurations { "debug", "release" }
  platforms { "native" }

  configuration "debug"
    flags { "Symbols" }
    targetdir "debug"

  configuration "dev"
    flags { "Symbols", "Optimize" }
    targetdir "dev"

  project "app"
    kind "ConsoleApp"
    language "C++"

    files { "./src/**.hpp", "./src/**.cpp", "./src/**.tpp" }

    includedirs { "./src/" }
    libdirs { }
    vpaths { ["*"] = "./src/**" }

    links { "ortools",
             "stdc++" }
    buildoptions { "-std=c++11" }

  configuration "debug"
    defines { "DEBUG" }
    targetname "debug"

  configuration "dev"
    targetname "dev"

