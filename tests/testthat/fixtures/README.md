# Test fixtures

## `bc-lha-population.csv`

Rows copied unchanged from the BC Stats local health area population resource
that `islh_bc_population("lha")` downloads (catalogue record
`86839277-986a-4a29-9f70-fa9b1166f6cb`, resource
`d4bbb2a0-aff7-403f-b52a-a634d05ee70f`, last modified 20 May 2026), retrieved
4 October 2026.

It keeps every column and the rows for Island Health's 14 local health areas,
one local health area elsewhere in BC (111) and the BC total (0), for 2025
(estimates) and 2026 (projections), and for female, male and total sex. The
tests use it to check the adapter against the catalogue's current schema
without a network request.

The data are published under the Open Government Licence - British Columbia.

To refresh it, download the resource again and keep the same rows. The live
check in `.github/workflows/bc-catalogue.yaml` reports when the schema
changes.
