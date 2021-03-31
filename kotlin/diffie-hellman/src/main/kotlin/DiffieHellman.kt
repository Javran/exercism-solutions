import java.math.BigInteger
import java.security.SecureRandom
import java.util.*

object DiffieHellman {

    val rnd: Random = SecureRandom()

    fun privateKey(prime: BigInteger): BigInteger =
        /*
          - Generate a random number of constrainted to same bitLength as `prime`,
          - then `mod (prime-2)`, which gives range [0, prime-3]
          - then `plus 2`, which gives intended range [2, prime-1]
         */
        BigInteger(prime.bitLength(), rnd).mod(prime.minus(BigInteger.TWO)).plus(BigInteger.TWO)

    fun publicKey(p: BigInteger, g: BigInteger, privKey: BigInteger): BigInteger = g.modPow(privKey, p)

    fun secret(prime: BigInteger, publicKey: BigInteger, privateKey: BigInteger): BigInteger =
        publicKey.modPow(privateKey, prime)
}
