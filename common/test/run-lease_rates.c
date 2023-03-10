#include "config.h"
#include "../amount.c"
#include "../lease_rates.c"
#include <common/setup.h>
#include <stdio.h>

/* AUTOGENERATED MOCKS START */
/* Generated stub for fromwire */
const u8 *fromwire(const u8 **cursor UNNEEDED, size_t *max UNNEEDED, void *copy UNNEEDED, size_t n UNNEEDED)
{ fprintf(stderr, "fromwire called!\n"); abort(); }
/* Generated stub for fromwire_bool */
bool fromwire_bool(const u8 **cursor UNNEEDED, size_t *max UNNEEDED)
{ fprintf(stderr, "fromwire_bool called!\n"); abort(); }
/* Generated stub for fromwire_fail */
void *fromwire_fail(const u8 **cursor UNNEEDED, size_t *max UNNEEDED)
{ fprintf(stderr, "fromwire_fail called!\n"); abort(); }
/* Generated stub for fromwire_lease_rates */
bool fromwire_lease_rates(const u8 **cursor UNNEEDED, size_t *plen UNNEEDED, struct lease_rates *lease_rates UNNEEDED)
{ fprintf(stderr, "fromwire_lease_rates called!\n"); abort(); }
/* Generated stub for fromwire_secp256k1_ecdsa_signature */
void fromwire_secp256k1_ecdsa_signature(const u8 **cursor UNNEEDED, size_t *max UNNEEDED,
					secp256k1_ecdsa_signature *signature UNNEEDED)
{ fprintf(stderr, "fromwire_secp256k1_ecdsa_signature called!\n"); abort(); }
/* Generated stub for fromwire_sha256 */
void fromwire_sha256(const u8 **cursor UNNEEDED, size_t *max UNNEEDED, struct sha256 *sha256 UNNEEDED)
{ fprintf(stderr, "fromwire_sha256 called!\n"); abort(); }
/* Generated stub for fromwire_tal_arrn */
u8 *fromwire_tal_arrn(const tal_t *ctx UNNEEDED,
		       const u8 **cursor UNNEEDED, size_t *max UNNEEDED, size_t num UNNEEDED)
{ fprintf(stderr, "fromwire_tal_arrn called!\n"); abort(); }
/* Generated stub for fromwire_u32 */
u32 fromwire_u32(const u8 **cursor UNNEEDED, size_t *max UNNEEDED)
{ fprintf(stderr, "fromwire_u32 called!\n"); abort(); }
/* Generated stub for fromwire_u64 */
u64 fromwire_u64(const u8 **cursor UNNEEDED, size_t *max UNNEEDED)
{ fprintf(stderr, "fromwire_u64 called!\n"); abort(); }
/* Generated stub for fromwire_u8 */
u8 fromwire_u8(const u8 **cursor UNNEEDED, size_t *max UNNEEDED)
{ fprintf(stderr, "fromwire_u8 called!\n"); abort(); }
/* Generated stub for fromwire_u8_array */
void fromwire_u8_array(const u8 **cursor UNNEEDED, size_t *max UNNEEDED, u8 *arr UNNEEDED, size_t num UNNEEDED)
{ fprintf(stderr, "fromwire_u8_array called!\n"); abort(); }
/* Generated stub for towire */
void towire(u8 **pptr UNNEEDED, const void *data UNNEEDED, size_t len UNNEEDED)
{ fprintf(stderr, "towire called!\n"); abort(); }
/* Generated stub for towire_bool */
void towire_bool(u8 **pptr UNNEEDED, bool v UNNEEDED)
{ fprintf(stderr, "towire_bool called!\n"); abort(); }
/* Generated stub for towire_lease_rates */
void towire_lease_rates(u8 **p UNNEEDED, const struct lease_rates *lease_rates UNNEEDED)
{ fprintf(stderr, "towire_lease_rates called!\n"); abort(); }
/* Generated stub for towire_secp256k1_ecdsa_signature */
void towire_secp256k1_ecdsa_signature(u8 **pptr UNNEEDED,
			      const secp256k1_ecdsa_signature *signature UNNEEDED)
{ fprintf(stderr, "towire_secp256k1_ecdsa_signature called!\n"); abort(); }
/* Generated stub for towire_sha256 */
void towire_sha256(u8 **pptr UNNEEDED, const struct sha256 *sha256 UNNEEDED)
{ fprintf(stderr, "towire_sha256 called!\n"); abort(); }
/* Generated stub for towire_u32 */
void towire_u32(u8 **pptr UNNEEDED, u32 v UNNEEDED)
{ fprintf(stderr, "towire_u32 called!\n"); abort(); }
/* Generated stub for towire_u64 */
void towire_u64(u8 **pptr UNNEEDED, u64 v UNNEEDED)
{ fprintf(stderr, "towire_u64 called!\n"); abort(); }
/* Generated stub for towire_u8 */
void towire_u8(u8 **pptr UNNEEDED, u8 v UNNEEDED)
{ fprintf(stderr, "towire_u8 called!\n"); abort(); }
/* Generated stub for towire_u8_array */
void towire_u8_array(u8 **pptr UNNEEDED, const u8 *arr UNNEEDED, size_t num UNNEEDED)
{ fprintf(stderr, "towire_u8_array called!\n"); abort(); }
/* AUTOGENERATED MOCKS END */

static void check_lease_rate_commitment_hash(void)
{
	struct pubkey p;
	struct sha256 sha;
	u8 *expected;
	assert(pubkey_from_hexstr("032cf15d1ad9c4a08d26eab1918f732d8ef8fdc6abb9640bf3db174372c491304e", strlen("032cf15d1ad9c4a08d26eab1918f732d8ef8fdc6abb9640bf3db174372c491304e"), &p));
	lease_rates_get_commitment(&p, 0, 0, 0, &sha);

	expected = tal_hexdata(tmpctx, "adc2973c151fecf212019dc3228a6765277cec6ab958959923839238a01cc1ce", strlen("adc2973c151fecf212019dc3228a6765277cec6ab958959923839238a01cc1ce"));

	assert(memeq(expected, tal_bytelen(expected),
		     &sha.u.u8, sizeof(sha.u.u8)));

	lease_rates_get_commitment(&p, 255, 100, 50, &sha);
	expected = tal_hexdata(tmpctx, "d363b338db30a4abd184e576773befb6a05ba61cec4169998ccbf546a720e421", strlen("d363b338db30a4abd184e576773befb6a05ba61cec4169998ccbf546a720e421"));

	assert(memeq(expected, tal_bytelen(expected),
		     &sha.u.u8, sizeof(sha.u.u8)));

}

static void check_lease_rate_fees(void)
{
	/* BOLT- #2:
	 * E.g.
	 * An node requests 1_000_000sats at a feerate of 2500perkw. They
	 * are contributing 500_000sats. Their weight contribution to the
	 * funding transaction will be 720.
	 *
	 * The accepter adds 1,100,000sats and charges a base funding fee of
	 * 233sats with a lease fee basis of 22. Their funding weight is 444.
	 * The lease fee is as follows:
	 * 233 + min(1_000_000,1_100_000) * 22 / 10_000 + 444 * 2500 / 1000
	 * The total lease fee for this open is 3543sats.
	 */
	struct amount_sat request_sats = amount_sat(1000000);
	struct amount_sat accepter_contrib = amount_sat(1100000);
	struct amount_sat fee;
	struct lease_rates rates;
	u32 feerate = 2500;

	rates.lease_fee_basis = 22;
	rates.lease_fee_base_sat = 233;
	rates.funding_weight = 444;

	assert(lease_rates_calc_fee(&rates, accepter_contrib,
				    request_sats, feerate,
				    &fee));
	assert(amount_sat_eq(fee, amount_sat(3543)));
}

int main(int argc, char *argv[])
{
	common_setup(argv[0]);

	check_lease_rate_commitment_hash();
	check_lease_rate_fees();

	common_shutdown();
}
