import com.jsuereth.pgp.sbtplugin.PgpKeys._

// Disable PGP for local builds
skip in pgpSigner := true
