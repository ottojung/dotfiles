(list (channel
       (name 'o-channel)
       (url "https://github.com/ottojung/o-channel.git")
       (branch "main")
       (introduction
        (make-channel-introduction
         "67a8015d374f3f933057e43b744293c1927edfbe"
         (openpgp-fingerprint
          "8B9B E811 5DCD EEFA 3E24  D5ED B851 B4F4 C06C 04D6"))))
      (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (branch "master")
       (commit "0a3a829e8f47aff0704e5664248d895fd47b6128")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (branch "master")
       (commit "4ae06fb5cb75f2ca6b0f2f384f41677ae28c069a")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
