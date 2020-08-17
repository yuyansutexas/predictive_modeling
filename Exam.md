# Visual Story Telling Part 1: Green Buildings

Described stats guru would be perfect if the green or not is the only attribute to impact the rent. Based on the question, there are actually confounders for the relationship between rent and green status ---- such as, **size, leasing_rate, stories, age, etc.**. They share the same property , which is that they are different from buildings in the same cluster. On the contrary, variables such as employment growth rate and costs are the same for both green_building and non_green building in the same cluster. Thus, our goal is to predict 

Let's do some quick plots!

We can see the difference is not obvious in class a and b. In class c, the rent of green-building is more than that of non-green building.

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-3-1.png)

For the net contract, the diffence between two groups is subtle. But the rent is higher in green-building group without net contract.

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-4-1.png)

In both, the rent on green-building is higher.

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-5-1.png)

Not a lot of useful information is gained from the scatter plot.

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-6-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-7-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-8-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-9-1.png)

Only predicting log(rent) on green_rating, we can see the rent higher for green building.



    Call:
    lm(formula = Rent ~ green_rating, data = df_green)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -25.465  -8.945  -3.245   5.735 221.555 

    Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    28.4448     0.1809 157.214  < 2e-16 ***
    green_rating1   1.5837     0.6062   2.612  0.00901 ** 
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Residual standard error: 15.13 on 7677 degrees of freedom
    Multiple R-squared:  0.0008882,	Adjusted R-squared:  0.000758 
    F-statistic: 6.825 on 1 and 7677 DF,  p-value: 0.009009



    Call:
    lm(formula = log(Rent) ~ . - class - size + log(size), data = df_green)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -2.14746 -0.27248 -0.01354  0.26452  2.17619 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    3.1662434  0.0769267  41.159  < 2e-16 ***
    leasing_rate   0.0042604  0.0003037  14.028  < 2e-16 ***
    stories        0.0005393  0.0006349   0.849  0.39564    
    age           -0.0004789  0.0001835  -2.610  0.00908 ** 
    class_a1       0.3260747  0.0213849  15.248  < 2e-16 ***
    class_b1       0.1001355  0.0166718   6.006 1.98e-09 ***
    green_rating1 -0.0257949  0.0179865  -1.434  0.15158    
    net1          -0.1978875  0.0266629  -7.422 1.28e-13 ***
    amenities1    -0.0208747  0.0116461  -1.792  0.07310 .  
    log(size)     -0.0361369  0.0074328  -4.862 1.19e-06 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    Residual standard error: 0.4285 on 7669 degrees of freedom
    Multiple R-squared:  0.0995,	Adjusted R-squared:  0.09844 
    F-statistic: 94.15 on 9 and 7669 DF,  p-value: < 2.2e-16



    size              Rent             leasing_rate     stories          age              class_a  class_b  green_rating
    Min.   :   2378   Min.   :  2.98   Min.   : 10.68   Min.   :  1.00   Min.   :  0.00   0:4544   0:4157   0:6995      
    1st Qu.:  52000   1st Qu.: 19.50   1st Qu.: 79.51   1st Qu.:  4.00   1st Qu.: 23.00   1:3135   1:3522   1: 684      
    Median : 132417   Median : 25.29   Median : 90.24   Median : 10.00   Median : 34.00                                 
    Mean   : 239465   Mean   : 28.59   Mean   : 84.88   Mean   : 13.83   Mean   : 47.04                                 
    3rd Qu.: 302375   3rd Qu.: 34.20   3rd Qu.: 96.66   3rd Qu.: 20.00   3rd Qu.: 79.00                                 
    Max.   :3781045   Max.   :250.00   Max.   :100.00   Max.   :110.00   Max.   :187.00
    
    net      amenities class          
    0:7406   0:3548    Length:7679       
    1: 273   1:4131    Class :character  
                       Mode  :character  



Based on stats guru, the size is 250000, greater than the mean, so the extra revenue should be higher. The location is pretty good (just across from downtown), assuming class a, higher rent. So I think there should be more than $2.60 per square foot per year. It is definitely promising investment.

# Visual Story Telling Part 2: Flights at ABIA

Notes: I tried to put multiple plots in one window ut it fails. I think it's becasue the bubble maps are interactive.

#########################

# Portfolio Modeling

I set a seed for this problem since I don't want the outcome would be different everytime.
Portfolio 1 includes three ETFs from oil & gas sectors.

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-31-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-32-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-32-2.png)

Portfolio 2

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-34-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-35-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-35-2.png)

Portfolio 3

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-37-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-38-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-38-2.png)

The first portforlio is all about the gas and oil ETFs. I would think it would have the least profit because it is not diversiied. Surprisingly, it has the highest VaR (13381) if we see it as positive number. I guess it might be because the oil and gas ETFs perform better these years. 

The second portfolio is consisted of ETFs of real estate, bond, and corporate bond. This portfolio would make about 5123 profit. I would say 5% in 20 days is still pretty good. 

The third portfolio includes two government bond ETFs, two currency ETFs, and each one from bond market, hedge fund, metal and oil&gas. I think this protfolio is the most diversified but it performs the worst out of the three. The VaR is 4034.

# Market Segmentation

I dropped columns __photo_sharing__ and __religion__ because I think they ust be overlapped with other categories. Before doing some clustering code, I regroup the 36 groups by similarity. Thus, there are only 12 groups now.  

* __Chatty__ group is the sum of __chatter__ and __uncategorized__. 

* __Up_to_date__ group is the sum of __current_events__, __politics__, and __news__.   

* Healthy&Outgoing group is the sum of __travel__, __healthy_nutrition__, __outdoors__, __automotive__, and __personal_fitness__.  

* __Homie__ group is the sum of __tv_film__, __music__, __online_gaming__, and __computers__.     

* __Sports__ group is the sum of __sports_fandom__ and __sports_playing__.      

* __Foodie__ group is the sum of __food__ and __cooking__.            

* __Businessman__ group is the sum of __business__ and __small_business__.  

* __Familish__ group is the sum of __family__, __home and garden__, and __parenting__.    

* __Shop__ group is the sum of __shopping__, __beauty__, and __fashion__.   

* __Young__ group is the sum of __college_uni__, __school__, and __dating__.  

* __Spam_user__ group is the sum of __spam__ and __adult__.            

* __Creative__ group is the sum of __crafts__ and __art__.



    [1] "cluster1's column means"
             chatter       up_to_date healthy_outgoing            homie           sports           foodie      businessman 
            4.741313         3.667954         7.258687         3.471042         1.976834         2.980695         0.957529 
            familish             shop            young        spam_user         creative          Cluster 
            2.745174         2.787645         2.841699         9.081081         1.463320         1.000000 
    [1] "cluster2's column means"
             chatter       up_to_date healthy_outgoing            homie           sports           foodie      businessman 
           4.4550517       10.3516309       18.0994431        3.8774861        2.3070804        4.1575179        0.9244232 
            familish             shop            young        spam_user         creative          Cluster 
           2.3357200        2.5393795        2.5147176        0.1034208        1.1424025        2.0000000 
    [1] "cluster3's column means"
             chatter       up_to_date healthy_outgoing            homie           sports           foodie      businessman 
           3.9844009        2.9761286        4.1800993        2.1448830        1.3164736        1.6868353        0.5010636 
            familish             shop            young        spam_user         creative          Cluster 
           1.4055779        1.9619475        1.5892224        0.1011581        0.7161428        3.0000000 
    [1] "cluster4's column means"
             chatter       up_to_date healthy_outgoing            homie           sports           foodie      businessman 
           3.9578125        3.8687500        6.4703125        3.3687500        7.2312500        6.2093750        0.8734375 
            familish             shop            young        spam_user         creative          Cluster 
           7.6921875        3.4281250        4.5734375        0.1734375        1.7812500        4.0000000 
    [1] "cluster5's column means"
             chatter       up_to_date healthy_outgoing            homie           sports           foodie      businessman 
           6.2960630        4.3748031        7.7448819        4.1354331        2.1448819       11.4031496        1.2267717 
            familish             shop            young        spam_user         creative          Cluster 
           2.5496063       11.4771654        3.3259843        0.1480315        1.5653543        5.0000000 
    [1] "cluster6's column means"
             chatter       up_to_date healthy_outgoing            homie           sports           foodie      businessman 
           5.1790698        4.4441860        6.4953488       10.2372093        3.0581395        2.8081395        1.3011628 
            familish             shop            young        spam_user         creative          Cluster 
           2.3709302        3.1011628        9.5430233        0.1337209        3.2558140        6.0000000 



After running the KMeans clustering model and looking at the statistics of each cluster, there are 6 clusters I got.      

* Cluster 1 is the group of followers who like posting tweets about sports and family.      

* Cluster 2 is the group of people who are concerned about current news, politics, and peosonal health and also like outdoors activity.   

* Cluster 3 is the group of people who like shopping and food and tweeting some random things.        

* Cluster 4 doesn't have some obvious patterns. I assume this is group of people who don'e reveal strong interests in some area.      

* Cluster 5 is the group of followers who are young, businessman, homebody, or  creative.          

* Cluster 6 is the spam group.

# Author Attribution

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-61-1.png)

I used three methods. It turned out that the random forest model performs way better than the other two. 

# Association Rule Mining

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-62-1.png)

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-63-1.png)

There are only 6 rules due to high support value(0.05) and low confidence value(0.1). We will fix that in the next part. We can notice that the most involved items are whole milk and rolls/buns, which are consistent with what we go from the frequency plot.

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-64-1.png)

Now there are 14 rules. We will fix that in the next part. However, whole milk still happens a lot of times.

![Image text](https://github.com/yuyansutexas/predictive_modeling/blob/master/figures/unnamed-chunk-65-1.png)

This time we get 100 rules. We found several rules:   

* People are likely to buy bottled beer if they buy reb/blush wine or liquor.     

* People are lkely ot buy tropical fruit if they buy vegetable juice, citrus fruit, or grape.
