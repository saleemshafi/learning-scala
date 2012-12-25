package rosalind

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import scala.math.BigDecimal.RoundingMode
import rosalind.SGRA._
 
class SGRATestSuite extends FunSuite {
 
  test("spectra graph with data from problem") {
    val data = """3524.8542
3623.5245
3710.9335
3841.974
3929.00603
3970.0326
4026.05879
4057.0646
4083.08025""".split("\r\n").map(a => BigDecimal.apply(a).setScale(4, RoundingMode.HALF_UP))

	expect("WMSPG") { getProteinStringFromMasses(data) }
  }

  test("spectra graph with real data") {
    val data = """82.4425417892
85.1399424109
151.365112453
179.495301789
308.537891789
409.585571789
445.266686427
506.638331789
537.805354797
634.733291789
693.234097174
763.351234071
781.801701789
859.48148441
896.828641789
950.161497027
953.555619026
1011.85558179
1142.55452053
1142.89607179
1190.58941988
1205.26148785
1255.98013179
1358.75084573
1385.02272179
1400.311996
1502.16922044
1532.09113179
1647.11807179
1734.15010179
1829.36526669
1862.20868179
1888.2055964
1976.25161179
2123.32002179
2132.35272179
2175.6864314
2210.35205179
2220.32130456
2245.43678179
2267.17860528
2358.52084179
2373.41538179
2461.53003179
2486.49944179
2518.55149179
2589.50863179
2646.53009179
2646.64645179
2687.94072951
2754.13659426
2783.70536179
2802.63120179
2873.66831179
2882.77377179
2915.06457807
2942.57303871
3019.83268179
3029.76942179
3040.73274484
3120.88036179
3166.82833179
3247.01849958
3283.94369179
3303.88724179
3397.02775179
3404.93492179
3429.90565632
3491.96695179
3537.61402654
3583.10706179
3611.36663752
3655.03028179
3720.16597179
3783.08886179
3833.25003179
3886.09805179
3961.34499179
3983.36062061
3999.18211179
4007.92587136
4058.39775179
4130.04401287
4152.4803671
4171.48181179
4242.51892179
4337.51083915
4343.56660179
4506.62993179
4528.56727245
4569.62029526
4603.68269179
4717.72562179""".split("\r\n").map(a => BigDecimal.apply(a))

	expect("PETPKFDDMIEFDSQNRIICGKHVHTYIWHIKPIATYPN") { getProteinStringFromMasses(data) }
  }
}