## Deskripsi Proyek

Proyek ini adalah simulasi server-pelanggan untuk skenario restoran atau kasir ukuran kecil. Dalam simulasi ini, diasumsikan bahwa pelanggan datang secara acak, dan waktu layanan setiap server juga bersifat acak. Jumlah customer dan server adalah konstan. Output berupa tabel yang mencakup **Customer #**, **Random Seed (Interarrival)**, **Time Between Arrivals**, **Arrival Time**, **Chosen Server**, **Random Seed (Server)**, **Service Time**, **Service Start**, **Service End**, dan **Wait Time** akan dihasilkan untuk mensimulasikan bagaimana proses pelayanan dilakukan.

Selain itu, user juga akan mendapatkan insights seperti rata-rata waktu antri, rata-rata total waktu customer, waktu idle setiap server, waktu rata-rata layanan setiap server, utilisasi setiap server, yang sangat berguna dalam memahami kinerja bisnis. Simulasi ini membantu restoran atau kasir dalam mengantisipasi apa yang akan terjadi selama operasional mereka, sehingga bisa lebih siap dalam menghadapi variasi jumlah pelanggan dan waktu pelayanan.

## Cara Menjalankan

1. Pastikan Haskell sudah diinstall pada komputer Anda. Jika belum, silakan kunjungi [https://downloads.haskell.org/~ghc/6.2/docs/html/users_guide/sec-installing-bin-distrib.html](https://downloads.haskell.org/~ghc/6.2/docs/html/users_guide/sec-installing-bin-distrib.html).
2. Jalankan perintah `cabal build`.
3. Jalankan perintah `cabal run`.
