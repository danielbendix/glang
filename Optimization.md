# Optimization

## Interleaving of identifier hashing

A quick experiment with interleaving identifier hashing with the scanning of identifier tokens seemed to lower the minimum parsing time by 5-10% in some cases. This makes sense as the ALU execution units are largely idle while checking characters for an identifier, and the hash constitutes a dependency for the initial load from the symbol table.

More measurement would need to be done to confirm this, and at the moment having hashing in several places could create hard-to-diagnose bugs if someone changes the hash function.

The experiment can be found in the `interleave_identifier_hashing` branch.
