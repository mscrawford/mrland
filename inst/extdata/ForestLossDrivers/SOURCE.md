# ForestLossDrivers - source record

`forest_loss.csv` is a hand transcription of **Table 1** of

> Curtis, P.G., Slay, C.M., Harris, N.L., Tyukavina, A., Hansen, M.C. (2018).
> Classifying drivers of global forest loss. *Science* 361(6407), 1108-1111.
> doi:10.1126/science.aau3445

Table 1 exists only inside the article. There is no machine-readable release of it, so
it cannot be fetched by a download function. The underlying gridded product - the 10 km
dominant-driver classification - *is* openly distributed, by Global Forest Watch under
CC BY 4.0, as the tiled raster set `tsc_tree_cover_loss_drivers`
(https://data-api.globalforestwatch.org/dataset/tsc_tree_cover_loss_drivers); the
successor 1 km product (Sims et al.) is on Zenodo at doi:10.5281/zenodo.14163025.
Deriving the regional totals from either raster would be a change of method and is out
of scope for this file.

## Columns

| column | meaning |
|---|---|
| `region` | Curtis source region (7 regions, Table 1 rows) |
| `treecoverloss_01_15` | total tree cover loss 2001-2015, Mha |
| `treecoverloss_pc_01_15` | that region's share of the global total, per cent |
| `deforestation` .. `urbanization` | share of *that region's* loss by driver, per cent |

`readForestLossDrivers()` converts the driver columns to Mha/yr as
`treecoverloss_01_15 * driver_pc / 100 / 15`. `treecoverloss_pc_01_15` is not used in
that calculation; it is retained as a consistency witness and is checked on read.

## Transcription record

Verified line by line against the article PDF on 2026-09-10. Table 1 as printed:

| region | Mha | % of global | deforest. | shifting | forestry | wildfire | urban |
|---|---|---|---|---|---|---|---|
| North America | 70 | 21% | 1% | <1% | 56% | 40% | 2% |
| Latin America | 78 | 25% | 56% | 31% | 13% | 1% | <1% |
| Europe | 15 | 5% | None | <1% | 99% | 1% | None |
| Africa | 39 | 13% | 4% | 92% | 4% | <1% | <1% |
| Russia/China/South Asia | 64 | 20% | <1% | <1% | 41% | 58% | <1% |
| Southeast Asia | 39 | 13% | 78% | 9% | 13% | <1% | <1% |
| Australia/Oceania | 10 | 3% | 7% | 10% | 29% | 53% | 1% |
| **Global** | **314** | **100%** | 25% | 21% | 31% | 22% | <1% |

Deviations of the CSV from the printed table, and their disposition:

1. **Africa `treecoverloss_01_15` = 3, should be 39.** A dropped digit. Corrected.
   This was not cosmetic: Africa is 92% shifting agriculture, which is the column MAgPIE
   consumes by default (`s35_forest_damage = 2`), so it made the SSA shifting-agriculture
   disturbance rate 13x too low from 2020 until this fix.
2. **Southeast Asia `treecoverloss_pc_01_15` = 12, should be 13.** Corrected. This
   column is not used in the calculation; with 12 the shares summed to 99 rather than 100.
3. **Europe: `shifting_agriculture` = 1 and `wildfire` = 0; the table has <1% and 1%
   respectively.** The 1 point sits on the wrong driver. Corrected separately - it moves
   a (very small) flow out of the default damage channel.
4. **Latin America `forestry` = 12, table says 13.** *Not* corrected. Latin America's
   printed driver shares sum to 101 because of the article's own rounding; shaving one
   point off forestry to reach 100 is a defensible normalisation, not a typo.
5. `<1%` and `None` are rendered as 0 or 1 rather than a fractional value, chosen so that
   each row's driver shares sum to 100. Left as-is; it is the same normalisation as (4).

The global total is 315 with Africa at 39, against the article's 314 - within the
rounding of the individual rows. With Africa at 3 it was 279, which is the check that
makes the error unambiguous.

## Mapping

`mapping.csv` assigns each ISO3 country to one of the seven Curtis source regions. It was
built from `madrat::regionmappingH12` (identical country set, 249 rows, same `;`-delimited
`X;CountryCode;RegionCode` layout) by rewriting the region column, and it carries at least
one artefact of that edit: the whole MEA block (ARE, BHR, DZA, EGY, IRN, IRQ, ISR, JOR,
KWT, LBN, LBY, MAR, OMN, PSE, QAT, SAU, SYR, TUN, YEM) is assigned to `Latin America`.
See the repository history for whether that has been corrected; the affected countries
hold very little natural forest, so the numerical consequence is small, but the
assignment is not defensible on its face.
