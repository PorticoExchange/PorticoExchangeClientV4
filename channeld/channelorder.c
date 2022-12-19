orderMessage  
{  
    tokenSell: “USDT”  
    amount: 20  
    networkA: CLN 
    tokenBuy: “BTC”  
    networkB: Sequentia
    ratio: {  
	USDT: 60000  
	BTC: 1  
	}  
    expirationTimestamp: 24*90 hours
    signature: signed by maker's private key
    nonce: to prevent replay attacks
}   

