module KeyManagement where

import Network.Solana.Core.Crypto

demoKeys :: IO ()
demoKeys = do
  
  -- Generate new key pair
  (pubKey, privKey) <- createSolanaKeyPair 


  -- Private key from list of 68 Bytes
  let myPrivKey1 =
        unsafeSolanaPrivateKeyRaw
          [22, 92, 4, 245, 212, 153, 201, 54, 28, 97, 151, 138, 173, 130, 57, 22, 83, 124, 200, 60, 185,
           35, 153, 114, 83, 74, 208, 151, 167, 200, 251, 120, 61, 127, 0, 228, 106, 0, 194, 74, 250, 228, 
           47, 77, 167, 219, 246, 233, 132, 0, 104, 179, 87, 70, 129, 53, 241, 74, 191, 236, 104, 204, 2, 113]
  
  -- Deriving public key (address) from the public key
  let myPubKey1 = toSolanaPublicKey myPrivKey1
  let myPubKey2 = unsafeSolanaPublicKey "594C9C199Zp8fK2zvmXrSveE359gijrM6tsoZLYk9obv"
  
  
  


  print myPubKey1 
  print myPubKey2 
  print myPubKey1 
  return ()