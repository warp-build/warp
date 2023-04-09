use std::env::current_dir;
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
    let cwd = current_dir().unwrap();

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
        .args([
            "build",
            "--print-hashes",
            "--print-cache-hits",
            "--force-redownload",
            "--skip-db",
            "--add-rule-dir",
            &cwd.join("../rules").to_string_lossy(),
            "--public-store-metadata-path",
            &cwd.join("../store").to_string_lossy(),
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
            "69ee8b77f4fa632c41309fa11ea75f24613689d1f307776175c44dc948d85f29",
            "d335ea86d12bce62398a73b7ab3f04fa2d1b5ad808537b43c492fa87d95a6a92",
            "8558cca30e94d49080d0c42b9cd427cc2ce902013035d45a9e65a0e2b4077c29",
            "ca1469c3c7b8b79236997f00a56d747bec7d67d5f684e585f840401bb1f0bc8b",
            "a037aed3f2d72982d0510b8f5689b37917b38f997e363b88bb8ff05e7473fc8a",
            "c845ee176c6cfbdfee8d632b3d403d99254857d074aae2c72cd85824da1de0ec",
            "e1f4373a9c38de29ba8d5c1325ecb7091612a1f63f46894ad475a0bb71db105f",
            "604a93573b2f6cb31627db29a14fc76b90102cd5dd1f8fcb1d3004312ee59846",
            "4bb38f1df59c4c2193efc5920a6a8daa02e20297c32cd3c80b120d0705420386",
            "b454fd3ae32cfa3f05fd954f24edd12eda55411484f588b5d3a31bb796afbc26",
            "989c73267f0ab1a58461b630fa2d12cbff84bf91701d46db06075c439195db06",
            "f9a406e2802206444b73845a2891a6ea549a4640f2f9ac9873a4d4f09d3a8db1",
            "ddf6551f891f9f15e3c39d118e5e0ce140f72ab48e61306adfa72e39382cd078",
            "453f36cfa13c2f6debecbf6bf97f3c3e304a72402864143a31c220bb34ff6116",
            "4cf40bfac85ed2a66513c734e3391a516038a0d465701f7afc056ad4bc9d7b6a",
            "73931ab586825cab00658c4378438e526526e3484b5004ed00e372702efc972f",
            "4ba690f2c820d101d0fcd9d3aa1e3d5eb2da369117af5f719d70ed2e75373a5d",
            "e42e51806564e853e7c4b436cccfc2dc6e431552fa29b3c150d33880f049fe26",
            "0aaa8cee85c502df69ba6796c66b24901272ba640bb38eaa2a9adff5e56ed942",
            "092105e060617b784db817a21890374d3d5c9eebad126f2bfd5640dc1f2438b6",
            "201e32b11d80226b4c348f24752469f55d88c67c8f9717013c0e6f37f42f3857",
            "1f0deb9dd47fe412d108e21c85607dc569347ad13c2ce8988a12a357338b2e06",
            "1a9ba362248c2d2631409bd3b6176508ecca4ddd60f084f876489513ce3b06f5"
        );
    };

    #[cfg(all(target_os = "macos", target_arch = "x86_64"))]
    {
        assert_in_store!(
            s,
            "131ecbb0230461df974899f0f7e2c0ba0631cad788e6c2dad2c5fc4e4e167f23",
            "16f41fd5b764f2c9b3ef972501b0e2aecd3d709bb7a75176ac2a301d350057b6",
            "2a8a08535a76804472038892fc80081c558566042241e5027f35a43e5fd587cc",
            "2bbd0e2b0fbfcb5ab04568e2e1ad6d147547463879af8ca95cb8f0382cf37eb8",
            "2e50a71fc89fff242d48a34baf9c9056b4fba763e535b1f79f35bc9ba4589ca3",
            "382c0399756ae9fef9ecbe94e2b9e7ea914b53d1ccaba8635f3b1f80b9575b21",
            "3870c9f068551ba410e21072fd9d12dfa6323ef0dfb3301715ed0900a8a3c414",
            "46ea95b466a4245f0eb3325e25c1cfd326ec400e7cdaec1d0f4e45bf04774b55",
            "52eca3eb150f6a5af275caabf4931734099d4bab16063225fe773ac8c5a6d1cd",
            "57a61bbd4fdc5867633985742c6fa452e9c18beb11cf38901b6162f068000be8",
            "63871713b8040e75f715bf5a2968e641cc186863776b1cd0f312417473a6f8f3",
            "64f99841ba878c93af7c6e52f7e017552e9ab4e5abe97411209a3d1382dd3ae8",
            "71b65b889048bffbde8d2dccc0d2d085c848343d08803e08b993503a78fcbf5f",
            "80488929ae79c2c0de40640e68e8b6026cfdbfffd307ad3ccca588dff6cc9c05",
            "93b12b32fa4451401f8b924613e1cd7595a6d86a180d5a671131084c8b067e2f",
            "9744945af955e06f2b6d277f55dc7faaa9d3ec425159782ea3b9fc85f882f5e7",
            "a1643a7ebd37160763c1f447b269e6f4a803620577384f23111af208e8314f1c",
            "b818b699a08d2ae40d9c32455810d9bfbfa32904e00d802bb6389eeba8277bee",
            "baf7e16386c160e375aff9fbd67ee745502a30d25abbf0230c1349cdc889ea70",
            "cd07577ceb655d1afd3664a9ca853c99079a67a2b7ec724d7761d89ebb9d7c1b",
            "da0eeffc4636bbc2d9aee86ed46d7acd62cfd1176ebc37c3cca3e2503f66e4a3",
            "da19a80831d9c0a09bc371de10a04382d75893408d10d9fc337c002d6d71b095",
            "ddc9986b0767f24d6022c17ad755b0107e839deafb7813f0d2e432ddbe69c181"
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
