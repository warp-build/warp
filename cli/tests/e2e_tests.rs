use std::path::PathBuf;
use std::process::Stdio;

macro_rules! assert_in_store {
    ( $s:expr,  $( $hash:expr ),* ) => {{
        $(
            assert!(std::fs::File::open($s.join($hash).join("Manifest.json")).is_ok());
        )*
    }};
}

#[test]
fn verl_build() {
    let warp_root = PathBuf::from("/warp");
    dbg!(&warp_root);

    let invocation_dir = PathBuf::from("./tests/repos/verl/").canonicalize().unwrap();
    dbg!(&invocation_dir);

    let mut warp = std::process::Command::new(assert_cmd::cargo::cargo_bin("warp"));

    let output = warp
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .args(["setup"])
        .output()
        .unwrap();

    assert!(output.status.success());

    let mut warp = std::process::Command::new(assert_cmd::cargo::cargo_bin("warp"));

    let output = warp
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .env("WARP_LOG", "debug")
        .env("WARP_TRICORDER_LOG", "debug")
        .args([
            "build",
            "--print-hashes",
            "--print-cache-hits",
            "--force-redownload",
            "--skip-db",
            "--warp-root",
            warp_root.to_string_lossy().as_ref(),
            "--invocation-dir",
            invocation_dir.to_string_lossy().as_ref(),
            "--max-localworkers",
            "0",
        ])
        .output()
        .unwrap();

    assert!(output.status.success());

    let s = warp_root.join("store");

    // NB(@ostera): these are all the expected hashes that you can see listed when running this
    // command outside on the test sources.
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    {
        assert_in_store!(
            s,
            "0410a5377e99c674d5649ebb895680fc77bd3bfe54005a1da4b16c9abbca25e2",
            "10f21733b8234bf40a7da53060d19e0c24ea0c0a5bcfddc4393c3192791b518a",
            "1bdb67efbe9e32611b219fb5b60e03003ada7b7e7a5cae6fe97f70a0f1c8275b",
            "1ee6a763470b94cc11b7e11a468da36edd5f0c143c7dfb5c65c869ce3f6d9736",
            "29631daf481a8f0fc06f5c286fbd056b7daf8198bcae07872e34cdb2e3c8dc07",
            "33eca79079eff4736375c808a19cf9194d2e0c1b86d0f2f877a39afb5f5c488f",
            "4082866b735750fbb12c6b3995873c23be32ce94e61feb24afc31ab38b7deabb",
            "412ec5fc87f758c8106d1ebd4fe1713a279ad0329be155041c846a7c16ab2410",
            "46385da35685aa6b08c03b8170351fd1d615e0819d4db652043ab9194518a80b",
            "5d2248353dac0feb890e0df1a9e311b1f325f486bd0507613628aefdb984fd7b",
            "61d25c4c6296751dfba23562e64a81c907098f69acb26f60e5d2223f0f8e2f52",
            "69ee8b77f4fa632c41309fa11ea75f24613689d1f307776175c44dc948d85f29",
            "736498429d0bf9d81827a8b5ff3ba8d39354f9e0ecf1e7dce05072f0a35b18cb",
            "7aee724258e93db5b095c097b331079c3db473439091ba759051faa9a22e4c21",
            "b454fd3ae32cfa3f05fd954f24edd12eda55411484f588b5d3a31bb796afbc26",
            "bed43c4f7593d0cbf8f7432bb5b889c02c10bbe9ec6d99beb1acabe86f72433d",
            "ca1469c3c7b8b79236997f00a56d747bec7d67d5f684e585f840401bb1f0bc8b",
            "d0f80d0ab39c8f0557477782330d7ffbac457e3884ad55dab112b99f1fc392d3",
            "d335ea86d12bce62398a73b7ab3f04fa2d1b5ad808537b43c492fa87d95a6a92",
            "d59c17c397015c813ae848f41f9d9f6a583c0ff89d454d9b5de3ee25fcfaa14d",
            "de5878570dd40c2171022c8dc3ab4b2a155f2d72c90f36414b93f17950e913b0",
            "e2d0e7279c50dfe4586be9ae9fca53528487565cd51a143d460ca6510e8735e2",
            "e86d9ea75beec1dd893c46d5aaaab164984a1b707f1531f3c1846e679a1e39e8"
        );
    };

    #[cfg(all(target_os = "macos", target_arch = "x86_64"))]
    {
        assert_in_store!(
            s,
            "00c39478f3f0123218585a263a0787feac7f0f5f5bfdd16e11badd4edd40d42d",
            "04499780a76fc5c4bd8ac69d473c86da44c686f756a5096f9c9e3b19ae8a62eb",
            "1a5b95862726dc879af106aa136ce809b46b256131f0acec2f06ec4a78164f4f",
            "1c8e7843b0bb63836d1e1767de32467efa6ea7d3238b37f0409aea099f403e50",
            "2bbd0e2b0fbfcb5ab04568e2e1ad6d147547463879af8ca95cb8f0382cf37eb8",
            "2c362796a5af25c8dee486692d24eec6b3478d9e08e254a293b7d84e89fd7daf",
            "37fa934f08a5ee6b9147abce4027ff971a20ffd505e0c2d5959dce67b71eac79",
            "3870c9f068551ba410e21072fd9d12dfa6323ef0dfb3301715ed0900a8a3c414",
            "46d9897cd0e79247f70b29bdc76920a5c8069bdf026738207edbccf843ee7aaa",
            "46ea95b466a4245f0eb3325e25c1cfd326ec400e7cdaec1d0f4e45bf04774b55",
            "52eca3eb150f6a5af275caabf4931734099d4bab16063225fe773ac8c5a6d1cd",
            "60b24851345ef748a630a5c846776a937ed2f796325d8f57783ae20124652efa",
            "64f99841ba878c93af7c6e52f7e017552e9ab4e5abe97411209a3d1382dd3ae8",
            "6820a95aec18132a97f3ebdf8d1803f83dc0857e3c03125cdb670788ef72215c",
            "71b65b889048bffbde8d2dccc0d2d085c848343d08803e08b993503a78fcbf5f",
            "80488929ae79c2c0de40640e68e8b6026cfdbfffd307ad3ccca588dff6cc9c05",
            "93b12b32fa4451401f8b924613e1cd7595a6d86a180d5a671131084c8b067e2f",
            "a1643a7ebd37160763c1f447b269e6f4a803620577384f23111af208e8314f1c",
            "b87102569bcef6d1fc71407108fed90dde58a1dce2ad2c5a5889ee5a58a4b34c",
            "d70d73546dba0b83621675c937dd7912082597ae9223ff4abb6fa3bfa273eb3d",
            "da0eeffc4636bbc2d9aee86ed46d7acd62cfd1176ebc37c3cca3e2503f66e4a3",
            "ddc9986b0767f24d6022c17ad755b0107e839deafb7813f0d2e432ddbe69c181",
            "f0dcc961d3d4bcb8ef718cea4ad1672a13ba0388c05e6a3163b239aa1841f0e8"
        );
    };

    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    {
        assert_in_store!(
            s,
            "4b92cf4658e04494f79ddf9ef81df7c7e27325a94cfbd2b179da20a280e1196f",
            "723ebf150bcd54f2cadd78351bbca09ebca7e37aef733e5d84242fbcbef30efb",
            "7254feb5ba8b4ff49591f18c85b4297fb91251326c36406fe57378d66ccf0dd5",
            "9623332961ca6ddf763ac202b17969127be8f699db0a95e5030b4808d9821157",
            "faeebf5a58382504fd9afc9196c224580b7fed82b706f36e55f82d2c5e6c5c28",
            "d22a686dc9633e3c028934e05c09a691de4654e64875a63560823fbcb89ea009",
            "aad4a6ebc180be1bd4e8da8e42349d434b04ed3b95ddbd0285883211299ce03c",
            "4942d6e024bc5b61afb26e4d92b03374ffb06bb686cf6ee5631eff58dde1117e",
            "02c02e05c48bc4658ce6046a00072567f762d9d2303645bf71e63ffdf24be728",
            "a3c2a07f9eb0bc2a184751031db35a66ce3002198b01c5af45ef21c5a2e83eb0",
            "7074c8d5f4cc9769d35a5e562bbe4e72e62bc8238466598178922bea022ad2ac",
            "210249aed409ca5fb411d56952f6b9dddabdf294da9122fd9f72d720dee10587",
            "aab82c97031ae8d5ca395e0f34e76776ac9ae7c84b8bb167021360b37abf3df4",
            "ecbcc8f9acd4db0930bc956ec61bb2ecb81b66dc26ad07d08dd2605345e09eb2",
            "17f7fc8b19ade416f998b82c61c4d78de835496cc546e10f3c9859a53a0c1b13",
            "c9abfa4ac6a55d4a9bbb27e7a2a5aac62868b4997e38c96ebf05a6c763bbccbb",
            "533b2904a6f048df22ab5d1cbb88e3b21711b2227e4802ce2531e6e0ac58a1d2",
            "f7faba7b24226f322e95deb62bf852897f2eaff0b74fc18600034c12d06760fa",
            "9684a085d09a76c652f1f2b7963b6bede3423b46e1616033f03393a0667760a7",
            "3c2eba0683367d3d5ec8393cc0d359ed8aff1f8ee9ae99485257ab69b80db5c4",
            "5020f8d351f3a20166d71b4fc8ef447f22aebe78b1683e1299b191fbcfc1328a"
        );
    };

    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    {
        assert_in_store!(
            s,
            "06016bc66dfc5d9a1b05074ee6f4ad4cf191da57ba43a4167a39cc02ab31b876",
            "07be599a1b62c749ebf99692463fa419c35f0c5f4d3336a1e6b374962b5dd245",
            "18240848fd076095e4cc9753a5643f03dcb12e003af0d248a9288e42a47aa24e",
            "1d8aca80bfe8236d7c7f84e24b38863f5908eb78b12689684e660c7b3cba3512",
            "2c7b80587ee3c4b22d0f9fd3742dc76cbe19e90742b3412a7f9eb4516e3a9a6a",
            "4b3d5a6485254c263351b6a5a9b249ca7f2d2cf4c3c90bca1358b685cf608300",
            "5c71a3c26fc09602ff5f374ef7eeab492e657bfc42d99e1f92a359c84007e14a",
            "5d2c47599d303ee48104d0fb768bf3bfb011ca0dc383b3c600d792e9261b82e8",
            "7e8d36a264f2e9141183235010beda1dc219cc3361e0566b79b9b09ed46a8126",
            "837952529a37428962b3920e968e1b9c7f972467817ac07713a7be0fbe85058b",
            "8618853b9ebd4674d38020aa599560cdc6d12b3d83a959959f6532e20d1dc5a4",
            "8e2054eafed5d90ebbd7e1f5e8c15fbc2b8f641e0b5739b807705cbcd71167d6",
            "971df8f02eb1ffb20c3e1d4ee76e103c9b42e0f8131ce4475822bbec4fe3e7e8",
            "a7653b82fed14804a9f2f5ce77f2cf5fc44b4196fbb398aeae94a1d50814b388",
            "aa6221089e9b02d42b6283125cc495811d8237c63558b566033f81ee83e3ed7d",
            "adf9a253693a12ae979b5464f54f9d57b02f75eaff0eaddba9265c2765da3726",
            "dd9704686bc07e730cf670e9e6da356e98ed352cee14fb69cb57be6520f0d866",
            "e9cc8c800f8696c7cedabde2d7470d43a4aa63e700414056172cdcb79de1da9c",
            "ea320cf840546fb068bdaed6fbca93ece73442da98b7a83062302b0681fbeb22",
            "ebca734d2503c794c64a7d078f6a2023ace4ce70aca37c882e7ee6b0a57ca86c",
            "ee7327ad2a7955c2060fec4d772ac9b20a72d525f3cfa91ae91075cc3d56a474",
            "ef47a34835bba72344c62ace50f0a8d39b1ade03750ebd47c556dbc653cb27ff",
            "f499584c16efc227f5e178f667be9f8aff3f1588f0bc74a8895f9782f54a5ab4"
        );
    };
}
