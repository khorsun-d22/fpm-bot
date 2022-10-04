{ pkgs, stdenv }:
rec {
  inherit (pkgs) eggDerivation fetchegg;

  address-info = eggDerivation {
    name = "address-info-1.0.5";

    src = fetchegg {
      name = "address-info";
      version = "1.0.5";
      sha256 = "1nv87ghfv8szmi2l0rybrgrds6fs5w6jxafqslnzw0mw5sfj6jyk";
    };

    buildInputs = [
      srfi-1
    ];
  };

  base64 = eggDerivation {
    name = "base64-1.0";

    src = fetchegg {
      name = "base64";
      version = "1.0";
      sha256 = "01lid9wxf94nr7gmskamxlfngack1hyxig8rl9swwgnbmz9qgysi";
    };

    buildInputs = [
      srfi-13
    ];
  };

  comparse = eggDerivation {
    name = "comparse-3";

    src = fetchegg {
      name = "comparse";
      version = "3";
      sha256 = "0xc6sa1690h4mfbgv82pmcildyk8y2cgfma8ih2gjkjwbm28p4c2";
    };

    buildInputs = [
      lazy-seq
      trie
      matchable
      srfi-1
      srfi-13
      srfi-14
      srfi-69
    ];
  };

  defstruct = eggDerivation {
    name = "defstruct-2.0";

    src = fetchegg {
      name = "defstruct";
      version = "2.0";
      sha256 = "0q1v1gdwqlpmwcsa4jnqldfqky9k7kvb83qgkhdyqym52bm5aln8";
    };

    buildInputs = [
      srfi-1
    ];
  };

  http-client = eggDerivation {
    name = "http-client-1.2.1";

    src = fetchegg {
      name = "http-client";
      version = "1.2.1";
      sha256 = "0n3wdz6kgkc8mj14wpsqawjygd9d9d65il8rk6b33sy9gsrqfp3s";
    };

    buildInputs = [
      intarweb
      uri-common
      simple-md5
      sendfile
      srfi-1
      srfi-13
      srfi-18
      srfi-69
    ];
  };

  intarweb = eggDerivation {
    name = "intarweb-2.0.2";

    src = fetchegg {
      name = "intarweb";
      version = "2.0.2";
      sha256 = "0iz3c1vv03hlsrgcpi0gcfzf8fzx2cwanwskn607na80xsnz714q";
    };

    buildInputs = [
      srfi-1
      srfi-13
      srfi-14
      defstruct
      uri-common
      base64
    ];
  };

  lazy-seq = eggDerivation {
    name = "lazy-seq-2";

    src = fetchegg {
      name = "lazy-seq";
      version = "2";
      sha256 = "07d0v97r49f49ic2y73jyqcqj67z5zgaifykafd9fclxraff4s3s";
    };

    buildInputs = [
      srfi-1
    ];
  };

  matchable = eggDerivation {
    name = "matchable-1.1";

    src = fetchegg {
      name = "matchable";
      version = "1.1";
      sha256 = "084hm5dvbvgnpb32ispkp3hjili8z02hamln860r99jx68jx6j2v";
    };

    buildInputs = [

    ];
  };

  medea = eggDerivation {
    name = "medea-4";

    src = fetchegg {
      name = "medea";
      version = "4";
      sha256 = "1rr8blml4j9xsgrw4cxjhz8c241vd9zrhp9d6mgc2sdjgcw1kl6v";
    };

    buildInputs = [
      comparse
      srfi-1
      srfi-13
      srfi-14
      srfi-69
    ];
  };

  memory-mapped-files = eggDerivation {
    name = "memory-mapped-files-0.4";

    src = fetchegg {
      name = "memory-mapped-files";
      version = "0.4";
      sha256 = "0by3r18bj9fs0bs9w5czx84vssmr58br3x7pz1m3myb4mns3mpsc";
    };

    buildInputs = [

    ];
  };

  openssl = eggDerivation {
    name = "openssl-2.2.4";

    src = fetchegg {
      name = "openssl";
      version = "2.2.4";
      sha256 = "0p2bc9vhxwm2nczr5m85r509gz7vnb5wyzmk1pby1vndfwplvrb2";
    };

    buildInputs = [
      srfi-1
      srfi-13
      srfi-18
      address-info
    ];
  };

  sendfile = eggDerivation {
    name = "sendfile-1.8.3";

    src = fetchegg {
      name = "sendfile";
      version = "1.8.3";
      sha256 = "0acmydjxlrbq7bdspmrzv9q9l3gh4xxnbpi5g1d5mz1g2mjwgm63";
    };

    buildInputs = [
      memory-mapped-files
    ];
  };

  simple-md5 = eggDerivation {
    name = "simple-md5-0.1.1";

    src = fetchegg {
      name = "simple-md5";
      version = "0.1.1";
      sha256 = "1sn8lijm4znmg80bk02wmvirdf4smxk4h2jsq5an5b6ycbdx8c4b";
    };

    buildInputs = [
      memory-mapped-files
      srfi-13
    ];
  };

  spiffy = eggDerivation {
    name = "spiffy-6.3";

    src = fetchegg {
      name = "spiffy";
      version = "6.3";
      sha256 = "0f22gfdyysgbm3q6cjibn1z1yavks3imxi1mxcyfmms3x91k5k3c";
    };

    buildInputs = [
      intarweb
      uri-common
      uri-generic
      sendfile
      srfi-1
      srfi-13
      srfi-14
      srfi-18
    ];
  };

  srfi-1 = eggDerivation {
    name = "srfi-1-0.5.1";

    src = fetchegg {
      name = "srfi-1";
      version = "0.5.1";
      sha256 = "15x0ajdkw5gb3vgs8flzh5g0pzl3wmcpf11iimlm67mw6fxc8p7j";
    };

    buildInputs = [

    ];
  };

  srfi-13 = eggDerivation {
    name = "srfi-13-0.3.3";

    src = fetchegg {
      name = "srfi-13";
      version = "0.3.3";
      sha256 = "09m424rwc76n9n9j8llhi70jjb47lfi2havpirq0rcvvgahfjwq7";
    };

    buildInputs = [
      srfi-14
    ];
  };

  srfi-133 = eggDerivation {
    name = "srfi-133-1.6.1";

    src = fetchegg {
      name = "srfi-133";
      version = "1.6.1";
      sha256 = "0c13cnb8v4p1mmi8rj9kgay9vq6n7vq9xxz4qprh265x1f3q4ikm";
    };

    buildInputs = [

    ];
  };

  srfi-14 = eggDerivation {
    name = "srfi-14-0.2.1";

    src = fetchegg {
      name = "srfi-14";
      version = "0.2.1";
      sha256 = "0gc33cx4xll9vsf7fm8jvn3gc0604kn3bbi6jfn6xscqp86kqb9p";
    };

    buildInputs = [

    ];
  };

  srfi-18 = eggDerivation {
    name = "srfi-18-0.1.6";

    src = fetchegg {
      name = "srfi-18";
      version = "0.1.6";
      sha256 = "00lykm5lqbrcxl3dab9dqwimpgm36v4ys2957k3vdlg4xdb1abfa";
    };

    buildInputs = [

    ];
  };

  srfi-69 = eggDerivation {
    name = "srfi-69-0.4.3";

    src = fetchegg {
      name = "srfi-69";
      version = "0.4.3";
      sha256 = "11pny54nc3rpmpaxcxs9dap1n6490y80zpwgfg0bwji1938a6fks";
    };

    buildInputs = [

    ];
  };

  sxml-serializer = eggDerivation {
    name = "sxml-serializer-0.5";

    src = fetchegg {
      name = "sxml-serializer";
      version = "0.5";
      sha256 = "1xjshbcac5xkl5mbkg7a350x8n36567i37dnrc0hbix2i2h19c8m";
    };

    buildInputs = [
      srfi-1
      srfi-13
    ];
  };

  trie = eggDerivation {
    name = "trie-2";

    src = fetchegg {
      name = "trie";
      version = "2";
      sha256 = "14pbwxn42ahq5vsfw36fmkhxd4kf86p6vhkbzd7529bafv135nwi";
    };

    buildInputs = [
      srfi-1
    ];
  };

  uri-common = eggDerivation {
    name = "uri-common-2.0";

    src = fetchegg {
      name = "uri-common";
      version = "2.0";
      sha256 = "07rq7ppkyk3i85vqspc048pnj6gmjhj236z00chslli9xybqkgrd";
    };

    buildInputs = [
      uri-generic
      defstruct
      matchable
      srfi-1
      srfi-13
      srfi-14
    ];
  };

  uri-generic = eggDerivation {
    name = "uri-generic-3.3";

    src = fetchegg {
      name = "uri-generic";
      version = "3.3";
      sha256 = "1aan62dac4pdsih362s1klj176yfswwqhizpgs4ryz71iycjczcd";
    };

    buildInputs = [
      matchable
      srfi-1
      srfi-14
    ];
  };
}

