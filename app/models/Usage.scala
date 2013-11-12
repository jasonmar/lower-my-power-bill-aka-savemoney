package models

import org.joda.time.DateTime
import scala.xml.XML
import scala.math.BigDecimal

case class Usage (custID:Int) {

    
    val xmlBaseDir = sys.env("HOME") + "/savemoney/public/"

    def parseXML (custID:Int) = {
        val fileName = custID.toString + ".XML"
        val xmlFile = xmlBaseDir + fileName

        val intervalData = scala.xml.XML.loadFile(xmlFile) \\ "IntervalBlock"
        intervalData
    }

    val intervalData = parseXML(custID)

    val data = intervalData.view.map(_ \\ "value").map(_.map(_.text.toInt)).toList

    val t = intervalData.view.map(_ \\ "start").map(_.map(_.text.toInt)).toList

    val dates = t.map (x => new DateTime(x.head.toLong * 1000))
    val month = dates.map (x => x.getMonthOfYear)
    val monthid = dates.map (x => x.getMonthOfYear + (x.getYear * 100) )
    val season = month.map (x => x match {
        case x if 5 to 10 contains x => 0
        case _ => 1
      }
    )


    /* Get list of months in current dataset */

    val months = monthid.distinct


    /* Get index for all meter reads for a month */

    def getIndexForMonthID(monthID:Int) = {
        monthid.zipWithIndex.filter(_._1 == monthID).map(_._2)
    }



    /* Tariff Configuration */

    val hours = List(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
    val hoursOffPeak = List(5,6,7,8,9,10,11,18,19,20,21,22,23)
    val hoursOnPeak = List(12,13,14,15,16,17)
    val hoursSuperOffPeak = List(0,1,2,3,4)
    val hoursMaxInsolation = List(11,12,13,14)
    val hoursHalfInsolation = List(9,10,15,16)



    /* prices in cents per kWh */

    val priceOffPeak = 19
    val priceOnPeak = 29
    val priceSuperOffPeak = 16

    val pricesByTierSummer = List(0,15,17,35,37)
    val pricesByTierWinter = List(0,15,17,33,35)

    val priceLeasedSolar = 15


    /* Baselines by Region */

    val baselineCoastal = List(294,498)
    val baselineInland = List(330,549)
    val baselineMountain = List(519,855)
    val baselineDesert = List(585,660)

    val tierThresholdsPct = List(0,100,130,200)







    /* Miscellaneous Usage Stats */

    val usageByDay = data.map(_.sum)
    val usageByHour = hours.map(num => data.map(_(num)))

    val avgUsagePerDay = usageByDay.sum / usageByDay.length
    val avgUsageByHour = usageByHour.map( usage => usage.sum / usage.length )

    val avgUsageOffPeak = hoursOffPeak.map(num => avgUsageByHour(num))
    val avgUsageOnPeak = hoursOnPeak.map(num => avgUsageByHour(num))
    val avgUsageSuperOffPeak = hoursSuperOffPeak.map(num => avgUsageByHour(num))



    /* Utility Function used by all Tariffs */

    def getMonthData(monthID:Int) = {
      val monthIDs = getIndexForMonthID(monthID)
      val monthData = monthIDs.map(id => data(id).toList)
      monthData
    }


    /* DR2 (default residential tariff) */

	def getMonthUsage(monthID:Int) = {
		val monthData = getMonthData(monthID)
		val monthUsage = monthData.map(_.sum).sum
		monthUsage
	}
	
	def getMonthlyBillDR2(monthID:Int, monthUsage:Int) = {
		val t1max = (baselineCoastal(1)).toInt
		val t2max = (baselineCoastal(1)*1.3).toInt
		val t3max = (baselineCoastal(1)*2).toInt
	
		val t1Usage = monthUsage match {
			case x if x > t1max => t1max
			case _ => monthUsage
		}
		val t2Usage = monthUsage match {
			case x if x > t2max => t2max - t1max
			case x if x < t1max => 0
			case _ => monthUsage - t1max
		}
		val t3Usage = monthUsage match {
			case x if x > t3max => t3max - t2max
			case x if x < t2max => 0
			case _ => monthUsage - t2max
		}
		val t4Usage = monthUsage match {
			case x if x > t3max => x - t3max
			case x if x < t3max => 0
			case _ => monthUsage - t3max
		}
		
		val t1Amt = t1Usage * pricesByTierSummer(1) / 100
		val t2Amt = t2Usage * pricesByTierSummer(2) / 100
		val t3Amt = t3Usage * pricesByTierSummer(3) / 100
		val t4Amt = t4Usage * pricesByTierSummer(4) / 100
		val totalUsage = t1Usage + t2Usage + t3Usage + t4Usage
		val totalAmt = t1Amt + t2Amt + t3Amt + t4Amt
		List(monthID, totalUsage, totalAmt, t1Usage, t1Amt, t2Usage, t2Amt, t3Usage, t3Amt, t4Usage, t4Amt)
	}
	
	def getDR2Bills() = {
        months.map(monthID => 
            getMonthlyBillDR2(monthID,getMonthUsage(monthID))
        )
    }
	
    val billsDR2 = getDR2Bills
    val billTotalAmtDR2 = billsDR2.map(x => x(2))
    val billTotalUsageDR2 = billsDR2.map(x => x(1))
    val avgBillAmtDR2 = billTotalAmtDR2.sum / billTotalAmtDR2.length
    val totalAmtDR2 = billTotalAmtDR2.sum
    val totalUsageDR2 = billTotalUsageDR2.sum
    val avgkWhPriceDR2 = BigDecimal(totalAmtDR2) / BigDecimal(totalUsageDR2)


    /* EVTOU2 Tariff */

    def getBillEVTOU2(hourlyUsage: List[Int]) = {
        val usageOffPeak = hoursOffPeak.map(hour => hourlyUsage(hour))
        val usageOnPeak = hoursOnPeak.map(hour => hourlyUsage(hour))
        val usageSuperOffPeak = hoursSuperOffPeak.map(hour => hourlyUsage(hour))

        val amtOffPeak = usageOffPeak.sum * priceOffPeak / 100
        val amtOnPeak = usageOnPeak.sum * priceOnPeak / 100
        val amtSuperOffPeak = usageSuperOffPeak.sum * priceSuperOffPeak / 100
        val total = amtOffPeak + amtOnPeak + amtSuperOffPeak
        val totalUsage = hourlyUsage.sum
        List(total, amtOffPeak, amtOnPeak, amtSuperOffPeak, usageOffPeak.sum, usageOnPeak.sum, usageSuperOffPeak.sum, totalUsage)
    }



    def getDailyAmtsEVTOU2(monthID: Int) = {
        val monthData = getMonthData(monthID)
        val dailyAmts = monthData.map(oneDayHourlyUsage => getBillEVTOU2(oneDayHourlyUsage))
        dailyAmts
    }

    def getMonthlyTotalEVTOU2(monthID: Int) = {
        val dailyAmts = getDailyAmtsEVTOU2(monthID)
        val monthTotal = dailyAmts.map(day => day(0)).sum
        val monthTotalOffPeak = dailyAmts.map(day => day(1)).sum
        val monthTotalOnPeak = dailyAmts.map(day => day(2)).sum
        val monthTotalSuperOffPeak = dailyAmts.map(day => day(3)).sum
        val monthTotalUsageOffPeak = dailyAmts.map(day => day(4)).sum
        val monthTotalUsageOnPeak = dailyAmts.map(day => day(5)).sum
        val monthTotalUsageSuperOffPeak = dailyAmts.map(day => day(6)).sum
        val monthTotalUsage = dailyAmts.map(day => day(7)).sum
        List(monthID,monthTotal,monthTotalOffPeak,monthTotalOnPeak,monthTotalSuperOffPeak,monthTotalUsageOffPeak,monthTotalUsageOnPeak,monthTotalUsageSuperOffPeak,monthTotalUsage)
    }


    /* Get TOU Bill totals for each available month */

    def getEVTOU2Bills () = {
        months.map(monthID => 
            getMonthlyTotalEVTOU2(monthID)
        )
    }
    val billsEVTOU2 = getEVTOU2Bills
    val billTotalAmtEVTOU2 = billsEVTOU2.map(x => x(1))
    val billTotalUsageEVTOU2 = billsEVTOU2.map(x => x(8))
    val avgBillAmtEVTOU2 = billTotalAmtEVTOU2.sum / billTotalAmtEVTOU2.length
    val totalAmtEVTOU2 = billTotalAmtEVTOU2.sum

    val totalUsageEVTOU2 = billTotalUsageEVTOU2.sum
    val avgkWhPriceEVTOU2 = BigDecimal(totalAmtEVTOU2) / BigDecimal(totalUsageEVTOU2)

    val savingsTOU = totalAmtDR2 - totalAmtEVTOU2
    val savingsPerkWhEVTOU2 = avgkWhPriceDR2 - avgkWhPriceEVTOU2


    /* Solar Savings Estimation */

    def getSolarByDay(monthID:Int, hourlyUsage: List[Int]) = {
        val usageDuringMaxInsolation = hoursMaxInsolation.map(hour => hourlyUsage(hour))
        val usageDuringHalfInsolation  = hoursHalfInsolation.map(hour => hourlyUsage(hour))

        val amtDuringMaxInsolation = usageDuringMaxInsolation.sum * priceLeasedSolar / 100
        val amtDuringHalfInsolation = usageDuringHalfInsolation.sum * priceLeasedSolar / 100

        val totalAmt = amtDuringMaxInsolation + amtDuringHalfInsolation
        val totalUsage = usageDuringMaxInsolation.sum + usageDuringMaxInsolation.sum

        List(monthID, totalUsage, totalAmt, usageDuringMaxInsolation.sum, amtDuringMaxInsolation, usageDuringHalfInsolation.sum, amtDuringHalfInsolation)
    }


    def getSolarByMonth(monthID: Int) = {
        val monthData = getMonthData(monthID)
        val dailyAmts = monthData.map(oneDayHourlyUsage => getSolarByDay(monthID,oneDayHourlyUsage))
        dailyAmts
    }

    def getSolarGuidance () = {
        val solarData = months.map(monthID =>
            getSolarByMonth(monthID)
        )
        val usageDuringMaxInsolation = solarData.map(_.map(_(3)))
        val usageDuringHalfInsolation = solarData.map(_.map(_(5)))

        val avgUsageDuringMaxInsolation = usageDuringMaxInsolation.map(x => x.sum / x.length )
        val minUsageDuringMaxInsolation = usageDuringMaxInsolation.map(x => x.min)
        val maxUsageDuringMaxInsolation = usageDuringMaxInsolation.map(x => x.max)

        val avgUsageDuringHalfInsolation = usageDuringHalfInsolation.map(x => x.sum / x.length )
        val minUsageDuringHalfInsolation = usageDuringHalfInsolation.map(x => x.min)
        val maxUsageDuringHalfInsolation = usageDuringHalfInsolation.map(x => x.max)

        val totalUsageDuringMaxInsolation = usageDuringMaxInsolation.map(_.sum).sum
        val totalUsageDuringHalfInsolation = usageDuringHalfInsolation.map(_.sum).sum

        val maxAvg = avgUsageDuringMaxInsolation.sum / avgUsageDuringMaxInsolation.length
        val maxAvgMin = minUsageDuringMaxInsolation.sum / minUsageDuringMaxInsolation.length
        val maxAvgMax = maxUsageDuringMaxInsolation.sum / maxUsageDuringMaxInsolation.length

        val halfAvg = avgUsageDuringHalfInsolation.sum / avgUsageDuringHalfInsolation.length
        val halfAvgMin = minUsageDuringHalfInsolation.sum / minUsageDuringHalfInsolation.length
        val halfAvgMax = maxUsageDuringHalfInsolation.sum / maxUsageDuringHalfInsolation.length

        /* Calculate suggested PV system size, with heaviest weighting on avgUsageDuringMaxInsolation */

        val suggestedPVSystemSize = maxAvgMin * 1000

        List(suggestedPVSystemSize: Int, totalUsageDuringMaxInsolation, totalUsageDuringHalfInsolation, maxAvg, maxAvgMin, maxAvgMax, halfAvg, halfAvgMin, halfAvgMax)

    }

    val solarGuidance = getSolarGuidance()
    val solarkWhSavingsEVTOU2 = avgkWhPriceEVTOU2 - (BigDecimal(priceLeasedSolar) / 100)
    val solarkWhSavingsDR2 = avgkWhPriceDR2 - (BigDecimal(priceLeasedSolar) / 100)

    val solarAmtSavingsEVTOU2 = ( 
        (BigDecimal(0.8) * solarGuidance(1) * solarkWhSavingsEVTOU2) 
        + (BigDecimal(0.4) * solarGuidance(2) * solarkWhSavingsEVTOU2) 
        ).setScale(2,  BigDecimal.RoundingMode.FLOOR)

    val solarAmtSavingsDR2 = ( 
        (BigDecimal(0.8) * solarGuidance(1) * solarkWhSavingsDR2) 
        + (BigDecimal(0.4) * solarGuidance(2) * solarkWhSavingsDR2) 
        ).setScale(2,  BigDecimal.RoundingMode.FLOOR)

}
