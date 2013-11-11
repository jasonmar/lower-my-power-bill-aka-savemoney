package models

import org.joda.time.DateTime
import scala.xml.XML
import java.math.BigDecimal

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
    val hoursOnPeak = List(12,13,14,16,17)
    val hoursSuperOffPeak = List(0,1,2,3,4)

    val priceOffPeak = 19
    val priceOnPeak = 29
    val priceSuperOffPeak = 16

    val pricesByTierSummer = List(0,15,17,35,37)
    val pricesByTierWinter = List(0,15,17,33,35)
    val tierThresholdsPct = List(0,100,130,200)


    /* Baselines by Region */

    val baselineCoastal = List(294,498)
    val baselineInland = List(330,549)
    val baselineMountain = List(519,855)
    val baselineDesert = List(585,660)

    /* prices in cents per kWh */






    /* Miscellaneous Usage Stats */

    val usageByDay = data.map(_.sum)
    val usageByHour = hours.map(num => data.map(_(num)))

    val avgUsagePerDay = usageByDay.sum / usageByDay.length
    val avgUsageByHour = usageByHour.map( usage => usage.sum / usage.length )

    val avgUsageOffPeak = hoursOffPeak.map(num => avgUsageByHour(num))
    val avgUsageOnPeak = hoursOnPeak.map(num => avgUsageByHour(num))
    val avgUsageSuperOffPeak = hoursSuperOffPeak.map(num => avgUsageByHour(num))




    def getMonthData(monthID:Int) = {
      val monthIDs = getIndexForMonthID(monthID)
      val monthData = monthIDs.map(id => data(id).toList)
      monthData
    }
	
	def getMonthUsage(monthID:Int) = {
		val monthData = getMonthData(monthID)
		val monthUsage = monthData.map(_.sum).sum
		monthUsage
	}
	
	def getMonthlyBillDR2(monthUsage:Int) = {
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
		List(totalUsage, totalAmt, t1Usage, t1Amt, t2Usage, t2Amt, t3Usage, t3Amt, t4Usage, t4Amt)
	}
	
	def getDR2Bills() = {
        months.map(monthID => 
            getMonthlyBillDR2(getMonthUsage(monthID))
        )
    }
	
    val billsDR2 = getDR2Bills
    val billTotalsDR2 = billsDR2.map(x => x(1))
    val avgBillAmtDR2 = billTotalsDR2.sum / billTotalsDR2.length
    val totalAmtDR2 = billTotalsDR2.sum


    def getBillEVTOU2(hourlyUsage: List[Int]) = {
        val usageOffPeak = hoursOffPeak.map(hour => hourlyUsage(hour))
        val usageOnPeak = hoursOnPeak.map(hour => hourlyUsage(hour))
        val usageSuperOffPeak = hoursSuperOffPeak.map(hour => hourlyUsage(hour))

        val amtOffPeak = usageOffPeak.sum * priceOffPeak / 100
        val amtOnPeak = usageOnPeak.sum * priceOnPeak / 100
        val amtSuperOffPeak = usageSuperOffPeak.sum * priceSuperOffPeak / 100
        val total = amtOffPeak + amtOnPeak + amtSuperOffPeak
        List(total, amtOffPeak, amtOnPeak, amtSuperOffPeak)
    }



    def getDailyAmtsEVTOU2(monthID: Int) = {
        val monthData = getMonthData(monthID)
        val dailyAmts = monthData.map(day => getBillEVTOU2(day))
        dailyAmts
    }

    def getMonthlyTotalEVTOU2(monthID: Int) = {
        val monthTotal = getDailyAmtsEVTOU2(monthID).map(day => day(0)).sum
        val monthTotalOffPeak = getDailyAmtsEVTOU2(monthID).map(day => day(1)).sum
        val monthTotalOnPeak = getDailyAmtsEVTOU2(monthID).map(day => day(2)).sum
        val monthTotalSuperOffPeak = getDailyAmtsEVTOU2(monthID).map(day => day(3)).sum
        List(monthTotal,monthTotalOffPeak,monthTotalOnPeak,monthTotalSuperOffPeak)
    }


    /* Get TOU Bill totals for each available month */

    def getEVTOU2Bills () = {
        months.map(monthID => 
            getMonthlyTotalEVTOU2(monthID)
        )
    }
    val billsEVTOU2 = getEVTOU2Bills
    val billTotalsEVTOU2 = billsEVTOU2.map(x => x(0))
    val avgBillAmtEVTOU2 = billTotalsEVTOU2.sum / billTotalsEVTOU2.length
    val totalAmtEVTOU2 = billTotalsEVTOU2.sum
	
	
	val savingsTOU = totalAmtDR2 - totalAmtEVTOU2

}
