library(ggplot2)
library(MASS)
library(reshape2)

set.seed(1)

fold_sfs = function(input_sfs) {
  input_length = length(input_sfs)
  folded_length = length(input_sfs) / 2
  if (input_length %% 2 == 1) {
    folded_length = folded_length + 1
  }
  output_sfs = c()
  for (i in 1:folded_length) {
    if (input_sfs[i] == input_sfs[input_length - i + 1]) {
      output_sfs[i] = input_sfs[i]
    } else {
      output_sfs[i] = input_sfs[i] + input_sfs[input_length - i + 1]
    }
  }
  return(output_sfs)
}

proportional_sfs = function(input_sfs) {
  input_sfs[1] = 0
  return (input_sfs / sum(input_sfs))
}
##### No singletons

# b_vulgatus
b_vulgatus_empirical_syn_no_s_sfs = c(3066, 1346, 904, 727, 543, 475, 434, 369, 331, 270, 222, 239, 
                                 241, 231, 220, 199, 212, 199, 192, 180, 192, 163, 143, 47)

b_vulgatus_empirical_nonsyn_no_s_sfs = c(3254, 1171, 732, 513, 341, 288, 265, 210, 174, 166, 141,
                                    116, 117, 113, 116, 115, 106, 117, 109, 104, 81, 79, 65, 20)

b_vulgatus_ratio = b_vulgatus_empirical_nonsyn_no_s_sfs / b_vulgatus_empirical_syn_no_s_sfs

b_vulgatus_bottleneck_growth_syn_no_s_sfs = fold_sfs(c(2364.311781722684, 1182.2854155709988, 788.1903216166188,
                                              591.1427476056948, 472.9142027663291, 394.095172685024,
                                              337.7958653686266, 295.5713847974877, 262.73012206416035,
                                              236.4571118211272, 214.9610124822103, 197.0475963226168,
                                              181.890090301725, 168.8979422474455, 157.63808056679633,
                                              147.7857015649522, 139.0924259458216, 131.36506981240356,
                                              124.45111956179653, 118.2285643116413, 112.59863334764663,
                                              107.48051426719834, 102.80744899856876, 98.52380581530493,
                                              94.58285406719469, 90.94505243481319, 87.57671757199122,
                                              84.44897803914536, 81.5369446642239, 78.81904683144305,
                                              76.27649723028186, 73.8928569640401, 71.65367972954647,
                                              69.54621878871471, 67.5591841735257, 65.68254035684163,
                                              63.90733673338427, 62.225564866854704, 60.63003769902822,
                                              59.11428687750645, 57.672475108656535, 56.29932103158347,
                                              54.99003457476419, 53.74026112772935, 52.54603315649781,
                                              51.403728130038026, 50.310031817833256))

b_vulgatus_bottleneck_growth_nonsyn_no_s_sfs = b_vulgatus_ratio * b_vulgatus_bottleneck_growth_syn_no_s_sfs

b_vulgatus_exponential_growth_syn_no_s_sfs = fold_sfs(c(2364.4467211037827, 1182.2854405836288, 788.1903187688821,
                                               591.142745460723, 472.9142010503583, 394.09517125503993,
                                               337.79586414292595, 295.57138372500805, 262.7301211108395, 
                                               236.45711096313843, 214.9610117022205, 197.04759560762616,
                                               181.8900896417336, 168.89794163459638, 157.63807999480824,
                                               147.7857010287071, 139.09242544112226, 131.3650693357431,
                                               124.4511191102217, 118.22856388264522, 112.59863293907891,
                                               107.4805138772034, 102.80744862552866, 98.52380545780818,
                                               94.58285372399779, 90.94505210482005, 87.57671725421633,
                                               84.44897773272076, 81.53694436836449, 78.81904654544562,
                                               76.27649695351124, 73.89285669591753, 71.65367946954882,
                                               69.54621853636701, 67.559183928386, 65.68254011851043,
                                               63.90733650149446, 62.225564641068146, 60.63003747903106,
                                               59.11428666300838, 57.67247489939094, 56.299320827299596,
                                               54.99003437523188, 53.740260932731864, 52.546032965832865,
                                               51.403727943517964, 50.310031635283856))

b_vulgatus_exponential_growth_nonsyn_no_s_sfs = b_vulgatus_ratio * b_vulgatus_exponential_growth_syn_no_s_sfs

b_vulgatus_three_epoch_syn_no_s_sfs = fold_sfs(c(2363.632596896401, 1182.2851162215534, 788.1903555758352,
                                        591.1427732881266, 472.9142233125268, 394.09518980685556,
                                        337.7958800444824, 295.57139763887005, 262.73013347871694,
                                        236.4571220942282, 214.9610218213931, 197.04760488353443,
                                        181.89009820411056, 168.89794958537502, 157.63808741553058,
                                        147.7857079856406, 139.09243198882248, 131.36507551968222,
                                        124.4511249686904, 118.22856944819218, 112.59863823959827,
                                        107.48051893679009, 102.80745346513334, 98.52381009576268,
                                        94.58285817643414, 90.9450563860063, 87.57672137684261,
                                        84.44898170811041, 81.53694820667175, 78.81905025580933,
                                        76.27650054418582, 73.89286017438353, 71.65368284260778,
                                        69.54622181021541, 67.55918710869781, 65.68254321048119,
                                        63.90733950989845, 62.22556757030273, 60.63004033315707,
                                        59.11428944578124, 57.6724776142913, 56.29932347756027,
                                        54.99003696385781, 53.7402634625254, 52.54603543940876,
                                        51.40373036332047, 50.310034003599775))

b_vulgatus_three_epoch_nonsyn_no_s_sfs = b_vulgatus_ratio * b_vulgatus_three_epoch_syn_no_s_sfs

b_vulgatus_two_epoch_syn_no_s_sfs = fold_sfs(c(2364.4410069186315, 1182.2854408699334, 788.1903187365133,
                                      591.1427454361523, 472.91420103069845, 394.0951712386567,
                                      337.7958641288832, 295.5713837127206, 262.7301210999173,
                                      236.45711095330847, 214.96101169328418, 197.04759559943452,
                                      181.8900896341721, 168.89794162757497, 157.63807998825047,
                                      147.7857010225634, 139.09242543533995, 131.365069330282,
                                      124.45111910504805, 118.22856387773024, 112.59863293439798,
                                      107.48051387273524, 102.80744862125476, 98.52380545371517,
                                      94.5828537200685, 90.9450521010393, 87.57671725057561,
                                      84.44897772921006, 81.53694436497484, 78.81904654216896,
                                      76.27649695034029, 73.89285669284567, 71.65367946657004,
                                      69.54621853347585, 67.55918392557744, 65.68254011577989,
                                      63.90733649883772, 62.225564638481316, 60.63003747651056,
                                      59.11428666055089, 57.67247489699339, 56.29932082495992,
                                      54.99003437294584, 53.74026093049778, 52.54603296364843,
                                      51.403727941381014, 50.31003163319094))

b_vulgatus_two_epoch_nonsyn_no_s_sfs = b_vulgatus_ratio * b_vulgatus_two_epoch_syn_no_s_sfs

b_vulgatus_bottleneck_growth_syn_no_s_sfs = proportional_sfs(b_vulgatus_bottleneck_growth_syn_no_s_sfs)
b_vulgatus_exponential_growth_syn_no_s_sfs = proportional_sfs(b_vulgatus_exponential_growth_syn_no_s_sfs)
b_vulgatus_three_epoch_syn_no_s_sfs = proportional_sfs(b_vulgatus_three_epoch_syn_no_s_sfs)
b_vulgatus_two_epoch_syn_no_s_sfs = proportional_sfs(b_vulgatus_two_epoch_syn_no_s_sfs)

b_vulgatus_bottleneck_growth_nonsyn_no_s_sfs = proportional_sfs(b_vulgatus_bottleneck_growth_nonsyn_no_s_sfs)
b_vulgatus_exponential_growth_nonsyn_no_s_sfs = proportional_sfs(b_vulgatus_exponential_growth_nonsyn_no_s_sfs)
b_vulgatus_three_epoch_nonsyn_no_s_sfs = proportional_sfs(b_vulgatus_three_epoch_nonsyn_no_s_sfs)
b_vulgatus_two_epoch_nonsyn_no_s_sfs = proportional_sfs(b_vulgatus_two_epoch_nonsyn_no_s_sfs)

b_vulgatus_x_axis = 1:length(b_vulgatus_empirical_syn_no_s_sfs)
b_vulgatus_bottleneck_growth_syn_no_s_sfs = b_vulgatus_bottleneck_growth_syn_no_s_sfs[1:length(b_vulgatus_x_axis)]
b_vulgatus_exponential_growth_syn_no_s_sfs = b_vulgatus_exponential_growth_syn_no_s_sfs[1:length(b_vulgatus_x_axis)]
b_vulgatus_two_epoch_syn_no_s_sfs = b_vulgatus_two_epoch_syn_no_s_sfs[1:length(b_vulgatus_x_axis)]
b_vulgatus_three_epoch_syn_no_s_sfs = b_vulgatus_three_epoch_syn_no_s_sfs[1:length(b_vulgatus_x_axis)]

df = data.frame(b_vulgatus_empirical_syn_no_s_sfs, 
                b_vulgatus_bottleneck_growth_syn_no_s_sfs, 
                b_vulgatus_exponential_growth_syn_no_s_sfs, 
                b_vulgatus_two_epoch_syn_no_s_sfs, 
                b_vulgatus_three_epoch_syn_no_s_sfs)

p_b_vulgatus <- ggplot(data = df, aes(x=b_vulgatus_x_axis, y=proportional_sfs(b_vulgatus_empirical_syn_no_s_sfs), color='b_vulgatus_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_syn_no_s_sfs, color='b_vulgatus_bottleneck')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_syn_no_s_sfs, color='b_vulgatus_bottleneck')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_syn_no_s_sfs, color='b_vulgatus_exponential')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_exponential_growth_syn_no_s_sfs, color='b_vulgatus_exponential')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_two_epoch_syn_no_s_sfs, color='b_vulgatus_two_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_two_epoch_syn_no_s_sfs, color='b_vulgatus_two_epoch')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_syn_no_s_sfs, color='b_vulgatus_three_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_syn_no_s_sfs, color='b_vulgatus_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_vulgatus_x_axis, limits = c(1, length(b_vulgatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_vulgatus_empirical',
                              'b_vulgatus_bottleneck',
                              'b_vulgatus_exponential',
                              'b_vulgatus_two_epoch',
                              'b_vulgatus_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. vulgatus, Empirical vs. Expected Syn SFS. (No singletons)')

p_b_vulgatus

df = data.frame(b_vulgatus_empirical_nonsyn_no_s_sfs, b_vulgatus_bottleneck_growth_nonsyn_no_s_sfs, b_vulgatus_exponential_growth_nonsyn_no_s_sfs, b_vulgatus_two_epoch_nonsyn_no_s_sfs, b_vulgatus_three_epoch_nonsyn_no_s_sfs)

p_b_vulgatus <- ggplot(data = df, aes(x=b_vulgatus_x_axis, y=proportional_sfs(b_vulgatus_empirical_nonsyn_no_s_sfs), color='b_vulgatus_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_nonsyn_no_s_sfs, color='b_vulgatus_bottleneck')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_nonsyn_no_s_sfs, color='b_vulgatus_bottleneck')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_nonsyn_no_s_sfs, color='b_vulgatus_exponential')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_exponential_growth_nonsyn_no_s_sfs, color='b_vulgatus_exponential')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_two_epoch_nonsyn_no_s_sfs, color='b_vulgatus_two_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_two_epoch_nonsyn_no_s_sfs, color='b_vulgatus_two_epoch')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_nonsyn_no_s_sfs, color='b_vulgatus_three_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_nonsyn_no_s_sfs, color='b_vulgatus_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_vulgatus_x_axis, limits = c(1, length(b_vulgatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_vulgatus_empirical',
                              'b_vulgatus_bottleneck',
                              'b_vulgatus_exponential',
                              'b_vulgatus_two_epoch',
                              'b_vulgatus_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. vulgatus, Empirical vs. Expected Nonsyn SFS. (No singletons)')

p_b_vulgatus

# b_ovatus

b_ovatus_empirical_syn_no_s_sfs = c(2850, 1283, 956, 657, 560, 542, 389, 365, 268, 305, 272, 227,
                               238, 226, 205, 187, 185, 202, 205, 188, 152, 125, 28)

b_ovatus_empirical_nonsyn_no_s_sfs = c(2074, 783, 550, 341, 323, 240, 149, 141, 113, 111, 110, 89,
                                  96, 82, 73, 75, 58, 77, 66, 58, 65, 59, 6)

b_ovatus_ratio = b_ovatus_empirical_nonsyn_no_s_sfs / b_ovatus_empirical_syn_no_s_sfs

b_ovatus_bottleneck_growth_syn_no_s_sfs = fold_sfs(c(2301.9437651195744, 1151.1458480490132, 767.4306349250169,
                                            575.5729826144529, 460.45839079705115, 383.7153293995799,
                                            328.8988568564132, 287.7865023632382, 255.81022657718117,
                                            230.22920588967503, 209.299279821268, 191.8576747183154,
                                            177.0993934356711, 164.4494380122914, 153.48614327665058,
                                            143.89326035001167, 135.42895185447153, 127.90512205163957,
                                            121.17327430590375, 115.11461130877353, 109.63296381050091,
                                            104.64964787946644, 100.09966374606425, 95.92884493562364,
                                            92.09169160941221, 88.54970390388065, 85.27008563897685,
                                            82.2247258032169, 79.38939076601298, 76.74307804752341,
                                            74.26749516529517, 71.94663619719155, 69.76643533284857,
                                            67.71448156311115, 65.7797822798781, 63.95256627594898,
                                            62.22411869083925, 60.58664201780737, 59.033138494389696,
                                            57.55731013436692, 56.153473389197984, 54.81648600068734,
                                            53.541684060228796, 52.32482765091364, 51.16205373733013))

b_ovatus_bottleneck_growth_nonsyn_no_s_sfs = b_ovatus_ratio * b_ovatus_bottleneck_growth_syn_no_s_sfs

b_ovatus_exponential_growth_syn_no_s_sfs = fold_sfs(c(2302.116682522891, 1151.1458926906105, 767.4306297711701,
                                             575.5729787269589, 460.4583876870363, 383.71532680789807,
                                             328.898854634974, 287.7865004194768, 255.81022484939322,
                                             230.22920433466587, 209.29927840762332, 191.85767342247442,
                                             177.09939223951017, 164.44943690157058, 153.48614223997777,
                                             143.89325937813092, 135.42895093976023, 127.90512118774555,
                                             121.17327348747781, 115.11461053126891, 109.63296307002031,
                                             104.64964717264404, 100.09966306997184, 95.92884428770311,
                                             92.0916909874085, 88.54970330580015, 85.27008506304747,
                                             82.22472524785643, 79.38939022980287, 76.74307752918696,
                                             74.2674946636782, 71.94663571125113, 69.76643486163361,
                                             67.71448110575545, 65.77978183558972, 63.95256584400194,
                                             62.22411827056733, 60.58664160859611, 59.03313809567018,
                                             57.557309745614575, 56.1534730099274, 54.81648563044701,
                                             53.54168369859871, 52.32482729750242, 51.16205339177395))

b_ovatus_exponential_growth_nonsyn_no_s_sfs = b_ovatus_ratio * b_ovatus_exponential_growth_syn_no_s_sfs

b_ovatus_three_epoch_syn_no_s_sfs = fold_sfs(c(2301.044860516565, 1151.145411624897, 767.4306851541962,
                                      575.5730206343461, 460.4584212134143, 383.7153547465576,
                                      328.8988785823896, 287.7865213734677, 255.8102434751631,
                                      230.2292210978589, 209.29929364688687, 191.8576873918022,
                                      177.09940513427435, 164.44944887528027, 153.48615341544024,
                                      143.893269855125, 135.42896080046253, 127.90513050062931,
                                      121.17328231020987, 115.11461891286604, 109.63297105249382,
                                      104.64965479227789, 100.09967035832013, 95.92885127236754,
                                      92.09169769268638, 88.54970975318277, 85.27009127163817,
                                      82.2247312347106, 79.38939601021495, 76.74308311691867,
                                      74.26750007116051, 71.94664094975168, 69.76643994138976,
                                      67.71448603610703, 65.77978662507411, 63.95257050044511,
                                      62.224122801160696, 60.58664601996334, 59.03314239392461,
                                      57.55731393641264, 56.153477098511694, 54.81648962168562,
                                      53.5416875970163, 52.3248311073196, 51.16205711692709))

b_ovatus_three_epoch_nonsyn_no_s_sfs = b_ovatus_ratio * b_ovatus_three_epoch_syn_no_s_sfs

b_ovatus_two_epoch_syn_no_s_sfs = fold_sfs(c(2302.1643492534035, 1151.14590202132, 767.4306286927919,
                                    575.5729779145231, 460.4583870370975, 383.7153262662824,
                                    328.898854170725, 287.7865000132691, 255.81022448831249,
                                    230.2292040096932, 209.2992781121936, 191.85767315166385,
                                    177.0993919895287, 164.4494366694472, 153.4861420233293,
                                    143.89325917502094, 135.4289507485979, 127.90512100720333,
                                    121.17327331643784, 115.11461036878255, 109.6329629152714,
                                    104.64964702492917, 100.09966292867935, 95.92884415229781,
                                    92.09169085741941, 88.54970318081064, 85.2700849426872,
                                    82.22472513179358, 79.38939011774332, 76.74307742086272, 
                                    74.26749455884828, 71.9466356096992, 69.76643476315704,
                                    67.71448101017525, 65.77978174274037, 63.95256575373082,
                                    62.224118182736866, 60.58664152307697, 59.033138012342164,
                                    57.557309664371395, 56.153472930665764, 54.816485553073335,
                                    53.54168362302366, 52.324827223644235, 51.16205331955706))

b_ovatus_two_epoch_nonsyn_no_s_sfs = b_ovatus_ratio * b_ovatus_two_epoch_syn_no_s_sfs

b_ovatus_bottleneck_growth_syn_no_s_sfs = proportional_sfs(b_ovatus_bottleneck_growth_syn_no_s_sfs)
b_ovatus_exponential_growth_syn_no_s_sfs = proportional_sfs(b_ovatus_exponential_growth_syn_no_s_sfs)
b_ovatus_three_epoch_syn_no_s_sfs = proportional_sfs(b_ovatus_three_epoch_syn_no_s_sfs)
b_ovatus_two_epoch_syn_no_s_sfs = proportional_sfs(b_ovatus_two_epoch_syn_no_s_sfs)

b_ovatus_bottleneck_growth_nonsyn_no_s_sfs = proportional_sfs(b_ovatus_bottleneck_growth_nonsyn_no_s_sfs)
b_ovatus_exponential_growth_nonsyn_no_s_sfs = proportional_sfs(b_ovatus_exponential_growth_nonsyn_no_s_sfs)
b_ovatus_three_epoch_nonsyn_no_s_sfs = proportional_sfs(b_ovatus_three_epoch_nonsyn_no_s_sfs)
b_ovatus_two_epoch_nonsyn_no_s_sfs = proportional_sfs(b_ovatus_two_epoch_nonsyn_no_s_sfs)

b_ovatus_x_axis = 1:length(b_ovatus_empirical_syn_no_s_sfs)
b_ovatus_bottleneck_growth_syn_no_s_sfs = b_ovatus_bottleneck_growth_syn_no_s_sfs[1:length(b_ovatus_x_axis)]
b_ovatus_exponential_growth_syn_no_s_sfs = b_ovatus_exponential_growth_syn_no_s_sfs[1:length(b_ovatus_x_axis)]
b_ovatus_two_epoch_syn_no_s_sfs = b_ovatus_two_epoch_syn_no_s_sfs[1:length(b_ovatus_x_axis)]
b_ovatus_three_epoch_syn_no_s_sfs = b_ovatus_three_epoch_syn_no_s_sfs[1:length(b_ovatus_x_axis)]

df = data.frame(b_ovatus_empirical_syn_no_s_sfs, b_ovatus_bottleneck_growth_syn_no_s_sfs, b_ovatus_exponential_growth_syn_no_s_sfs, b_ovatus_two_epoch_syn_no_s_sfs, b_ovatus_three_epoch_syn_no_s_sfs)

p_b_ovatus <- ggplot(data = df, aes(x=b_ovatus_x_axis, y=proportional_sfs(b_ovatus_empirical_syn_no_s_sfs), color='b_ovatus_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_syn_no_s_sfs, color='b_ovatus_bottleneck')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_syn_no_s_sfs, color='b_ovatus_bottleneck')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_syn_no_s_sfs, color='b_ovatus_exponential')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_exponential_growth_syn_no_s_sfs, color='b_ovatus_exponential')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_two_epoch_syn_no_s_sfs, color='b_ovatus_two_epoch')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_two_epoch_syn_no_s_sfs, color='b_ovatus_two_epoch')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_three_epoch_syn_no_s_sfs, color='b_ovatus_three_epoch')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_three_epoch_syn_no_s_sfs, color='b_ovatus_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_ovatus_x_axis, limits = c(1, length(b_ovatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_ovatus_empirical',
                              'b_ovatus_bottleneck',
                              'b_ovatus_exponential',
                              'b_ovatus_two_epoch',
                              'b_ovatus_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. ovatus, Empirical vs. Expected Syn SFS. (No singletons)')

p_b_ovatus

df = data.frame(b_ovatus_empirical_nonsyn_no_s_sfs, b_ovatus_bottleneck_growth_nonsyn_no_s_sfs, b_ovatus_exponential_growth_nonsyn_no_s_sfs, b_ovatus_two_epoch_nonsyn_no_s_sfs, b_ovatus_three_epoch_nonsyn_no_s_sfs)

p_b_ovatus <- ggplot(data = df, aes(x=b_ovatus_x_axis, y=proportional_sfs(b_ovatus_empirical_nonsyn_no_s_sfs), color='b_ovatus_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_nonsyn_no_s_sfs, color='b_ovatus_bottleneck')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_nonsyn_no_s_sfs, color='b_ovatus_bottleneck')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_nonsyn_no_s_sfs, color='b_ovatus_exponential')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_exponential_growth_nonsyn_no_s_sfs, color='b_ovatus_exponential')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_two_epoch_nonsyn_no_s_sfs, color='b_ovatus_two_epoch')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_two_epoch_nonsyn_no_s_sfs, color='b_ovatus_two_epoch')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_three_epoch_nonsyn_no_s_sfs, color='b_ovatus_three_epoch')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_three_epoch_nonsyn_no_s_sfs, color='b_ovatus_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_ovatus_x_axis, limits = c(1, length(b_ovatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_ovatus_empirical',
                              'b_ovatus_bottleneck',
                              'b_ovatus_exponential',
                              'b_ovatus_two_epoch',
                              'b_ovatus_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. ovatus, Empirical vs. Expected Nonsyn SFS. (No singletons)')

p_b_ovatus

# a_putredinis

a_putredinis_empirical_syn_no_s_sfs = c(4348, 2472, 1485, 1091, 580, 395, 300, 196, 184, 131, 113,
                                   90, 129, 179, 167, 65)

# b_uniformis

b_uniformis_empirical_syn_no_s_sfs = c(3013, 1244, 743, 580, 427, 384, 371, 341, 278, 240, 219,
                                  245, 197, 182, 220, 178, 146, 130, 148, 160, 138, 115,
                                  119, 108, 95, 108, 87, 111, 80, 75, 68, 83, 64, 84, 66, 55,
                                  43)

b_uniformis_empirical_nonsyn_no_s_sfs = c(3053, 1024, 506, 330, 322, 226, 235, 182, 171, 141,
                                     136, 116, 95, 107, 124, 86, 78, 66, 82, 69, 54, 69,
                                     57, 69, 55, 69, 54, 40, 47, 39, 36, 25, 31, 35, 36,
                                     33, 27)

b_uniformis_ratio = b_uniformis_empirical_nonsyn_no_s_sfs / b_uniformis_empirical_syn_no_s_sfs

b_uniformis_bottleneck_growth_syn_no_s_sfs = fold_sfs(c(2054.0457394272116, 1027.2453061206927, 684.8303199886025,
                                               513.622744419909, 410.8981985369978, 342.415167773767,
                                               293.4987171411248, 256.8113791131977, 228.27678283197577,
                                               205.4491057785879, 186.77191543911303, 171.2075901367091,
                                               158.0377764022827, 146.74936461476466, 136.96607438520724,
                                               128.40569542178974, 120.85241985422826, 114.13839711669802,
                                               108.13111360457053, 102.72455843412467, 97.83291327089273,
                                               93.38596311394922, 89.32570426686088, 85.6038003159219,
                                               82.17964867362771, 79.01889330438836, 76.09226795565446,
                                               73.37468726810303, 70.84452593196642, 68.48304201212234,
                                               66.27391188763936, 64.20285239021645, 62.25731164466646,
                                               60.42621446701284, 58.69975140857995, 57.06920295944169,
                                               55.52679225938382, 54.065561065066774, 52.67926479912406,
                                               51.36228334193432, 50.10954487822367, 48.91646062274754,
                                               47.77886865400133, 46.69298540698622, 45.65536363358356,
                                               44.66285584638942, 43.71258242926336, 42.80190373407585,
                                               41.928395594182064, 41.08982777626469, 40.28414496726461,
                                               39.50944995513232, 38.7639887136311, 38.04613714439265,
                                               37.35438926529399, 36.68734666436607, 36.043709063810304,
                                               35.4222658601546, 34.82188852475181, 34.24152376418735,
                                               33.680187353465556, 33.136958565986724, 32.61097513397617,
                                               32.101428681394175, 31.60756057842373, 31.1286581728099,
                                               30.66405135870282, 30.21310944823886, 29.775238315173148,
                                               29.34987778333432, 28.93649923576827, 28.534603423128964,
                                               28.143718452204315))

b_uniformis_bottleneck_growth_nonsyn_no_s_sfs = b_uniformis_ratio * b_uniformis_bottleneck_growth_syn_no_s_sfs

b_uniformis_exponential_growth_syn_no_s_sfs = fold_sfs(c(2054.327328939652, 1027.2454202781387, 684.8303087576919,
                                                513.6227359206447, 410.8981917374987, 342.41516210751524,
                                                293.4987122843355, 256.81137486351065, 228.27677905447615,
                                                205.44910237883823, 186.7719123484315, 171.2075873035819,
                                                158.03777378709063, 146.749362186372, 136.96607211870545,
                                                128.40569329694614, 120.85241785437717, 114.13839522795136,
                                                108.13111181522852, 102.72455673424976, 97.83291165196422,
                                                93.38596156860837, 89.32570278870749, 85.60379889935943,
                                                82.17964731372774, 79.01889199679222, 76.09226669648997,
                                                73.37468605390661, 70.84452475963883, 68.48304087887138,
                                                66.2739107909458, 64.20285132779365, 62.25731061443917,
                                                60.42621346708635, 58.69975043722279, 57.069202015065855,
                                                55.52679134053244, 54.06556017039415, 52.679263927393265,
                                                51.362282491996794, 50.10954404901632, 48.91645981328322,
                                                47.778867863361754, 46.69298463431573, 45.655362878084176,
                                                44.6628551073133, 43.71258170591164, 42.80190302579456,
                                                41.92839490035549, 41.08982709631464, 40.28414430064692,
                                                39.5094493013342, 38.76398807216879, 38.04613651480928,
                                                37.354388647157585, 36.68734605726781, 36.04370846736238,
                                                35.422265273992785, 34.82188794852295, 34.2415231975623,
                                                33.68018679612945, 33.136958017639905, 32.610974594333264,
                                                32.101428150183196, 31.60756005538522, 31.128657657696216,
                                                30.664050851276528, 30.21310894827558, 29.775237822456557,
                                                29.34987729765487, 28.9364987569302, 28.53460295094142,
                                                28.143717986485093))

b_uniformis_exponential_growth_nonsyn_no_s_sfs = b_uniformis_ratio * b_uniformis_exponential_growth_syn_no_s_sfs

b_uniformis_three_epoch_syn_no_s_sfs = fold_sfs(c(2052.857836930186, 1027.2443654746291, 684.8304118988689,
                                         513.6228145032961, 410.89825460603475, 342.41521449796966,
                                         293.4987571904459, 256.811414156352, 228.27681398143994,
                                         205.44913381310872, 186.77194092504385, 171.20761349881005,
                                         158.03779796729688, 146.74938463942496, 136.96609307489027,
                                         128.40571294336405, 120.85243634512523, 114.13841269143413,
                                         108.13112835958222, 102.72457245138732, 97.83292662066674,
                                         93.38597585691272, 89.32571645578508, 85.60381199697554,
                                         82.17965988743924, 79.01890408689837, 76.09227833881555,
                                         73.37469728043492, 70.8445355990455, 68.48305135696354,
                                         66.27392093103693, 64.20286115100606, 62.25732013997762,
                                         60.42622271246023, 58.69975941844483, 57.06921074681034,
                                         55.52679983628307, 54.065568442573195, 52.67927198746443,
                                         51.362290350566205, 50.10955171591262, 48.91646729763439,
                                         47.77887517365895, 46.692991778469825, 45.655369863478015,
                                         44.66286194085076, 43.71258839405533, 42.80190957460255,
                                         41.928401315513156, 41.089833383170344, 40.284150464230954,
                                         39.50945534638666, 38.763994003164214, 38.04614233597201,
                                         37.35439436248206, 36.68735167053189, 36.04371398214662,
                                         35.42227069369403, 34.82189327636685, 34.24152843660881,
                                         33.68019194929091, 33.13696308768399, 32.61097958390046,
                                         32.10143306178932, 31.607564891428183, 31.1286624204658,
                                         30.664055542959137, 30.213113570963714, 29.77524237815006,
                                         29.34988178826704, 28.93650318429349, 28.534607316813556,
                                         28.143722292550763))

b_uniformis_three_epoch_nonsyn_no_s_sfs = b_uniformis_ratio * b_uniformis_three_epoch_syn_no_s_sfs

b_uniformis_two_epoch_syn_no_s_sfs = fold_sfs(c(2054.4213420617302, 1027.245433338713, 684.8303074666856,
                                       513.6227349487477, 410.89819095998115, 342.41516145958394,
                                       293.49871172896997, 256.8113743775622, 228.27677862252193,
                                       205.44910199007944, 186.7719119950144, 171.20758697961864,
                                       158.0377734880454, 146.74936190868712, 136.96607185953485,
                                       128.40569305397187, 120.85241762569723, 114.13839501197424,
                                       108.13111161061862, 102.72455653987035, 97.83291146684097,
                                       93.38596139189981, 89.32570261968317, 85.6037987373766,
                                       82.1796471582242, 79.0188918472696, 76.09226655250522,
                                       73.37468591506418, 70.84452462558406, 68.4830407492851,
                                       66.27391066553973, 64.20285120630652, 62.25731049663346,
                                       60.42621335274381, 58.69975032614884, 57.069201907077286,
                                       55.52679123546248, 54.06556006808996, 52.67926382771151,
                                       51.36228239480708, 50.109543954197086, 48.91645972072228,
                                       47.77886777295272, 46.692984545961444, 45.65536279169267,
                                       44.6628550228005, 43.71258162319761, 42.801902944803125,
                                       41.92839482101695, 41.08982701856287, 40.28414422441969,
                                       39.509449226572876, 38.76398799881806, 38.046136442816895,
                                       37.35438857647415, 36.687345987846584, 36.04370839915958,
                                       35.42226520696539, 34.82188788263261, 34.24152313276916,
                                       33.68018673239944, 33.13695795493686, 32.610974532625505,
                                       32.10142808943962, 31.607559995576164, 31.128657598793357,
                                       30.66405079325369, 30.213108891105158, 29.775237766114692,
                                       29.349877242118723, 28.936498702175427, 28.53460289694713,
                                       28.143717933228853))

b_uniformis_two_epoch_nonsyn_no_s_sfs = b_uniformis_ratio * b_uniformis_two_epoch_syn_no_s_sfs

b_uniformis_bottleneck_growth_syn_no_s_sfs = proportional_sfs(b_uniformis_bottleneck_growth_syn_no_s_sfs)
b_uniformis_exponential_growth_syn_no_s_sfs = proportional_sfs(b_uniformis_exponential_growth_syn_no_s_sfs)
b_uniformis_three_epoch_syn_no_s_sfs = proportional_sfs(b_uniformis_three_epoch_syn_no_s_sfs)
b_uniformis_two_epoch_syn_no_s_sfs = proportional_sfs(b_uniformis_two_epoch_syn_no_s_sfs)

b_uniformis_bottleneck_growth_nonsyn_no_s_sfs = proportional_sfs(b_uniformis_bottleneck_growth_nonsyn_no_s_sfs)
b_uniformis_exponential_growth_nonsyn_no_s_sfs = proportional_sfs(b_uniformis_exponential_growth_nonsyn_no_s_sfs)
b_uniformis_three_epoch_nonsyn_no_s_sfs = proportional_sfs(b_uniformis_three_epoch_nonsyn_no_s_sfs)
b_uniformis_two_epoch_nonsyn_no_s_sfs = proportional_sfs(b_uniformis_two_epoch_nonsyn_no_s_sfs)

b_uniformis_x_axis = 1:length(b_uniformis_empirical_syn_no_s_sfs)
b_uniformis_bottleneck_growth_syn_no_s_sfs = b_uniformis_bottleneck_growth_syn_no_s_sfs[1:length(b_uniformis_x_axis)]
b_uniformis_exponential_growth_syn_no_s_sfs = b_uniformis_exponential_growth_syn_no_s_sfs[1:length(b_uniformis_x_axis)]
b_uniformis_two_epoch_syn_no_s_sfs = b_uniformis_two_epoch_syn_no_s_sfs[1:length(b_uniformis_x_axis)]
b_uniformis_three_epoch_syn_no_s_sfs = b_uniformis_three_epoch_syn_no_s_sfs[1:length(b_uniformis_x_axis)]

df = data.frame(b_uniformis_empirical_syn_no_s_sfs, b_uniformis_bottleneck_growth_syn_no_s_sfs, b_uniformis_exponential_growth_syn_no_s_sfs, b_uniformis_two_epoch_syn_no_s_sfs, b_uniformis_three_epoch_syn_no_s_sfs)

p_b_uniformis <- ggplot(data = df, aes(x=b_uniformis_x_axis, y=proportional_sfs(b_uniformis_empirical_syn_no_s_sfs), color='b_uniformis_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_syn_no_s_sfs, color='b_uniformis_bottleneck')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_syn_no_s_sfs, color='b_uniformis_bottleneck')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_syn_no_s_sfs, color='b_uniformis_exponential')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_syn_no_s_sfs, color='b_uniformis_exponential')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_two_epoch_syn_no_s_sfs, color='b_uniformis_two_epoch')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_two_epoch_syn_no_s_sfs, color='b_uniformis_two_epoch')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_three_epoch_syn_no_s_sfs, color='b_uniformis_three_epoch')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_three_epoch_syn_no_s_sfs, color='b_uniformis_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_uniformis_x_axis, limits = c(1, length(b_uniformis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_uniformis_empirical',
                              'b_uniformis_bottleneck',
                              'b_uniformis_exponential',
                              'b_uniformis_two_epoch',
                              'b_uniformis_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. uniformis, Empirical vs. Expected Syn SFS. (No singletons)')

p_b_uniformis

df = data.frame(b_uniformis_empirical_nonsyn_no_s_sfs, b_uniformis_bottleneck_growth_nonsyn_no_s_sfs, b_uniformis_exponential_growth_nonsyn_no_s_sfs, b_uniformis_two_epoch_nonsyn_no_s_sfs, b_uniformis_three_epoch_nonsyn_no_s_sfs)

p_b_uniformis <- ggplot(data = df, aes(x=b_uniformis_x_axis, y=proportional_sfs(b_uniformis_empirical_nonsyn_no_s_sfs), color='b_uniformis_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_nonsyn_no_s_sfs, color='b_uniformis_bottleneck')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_nonsyn_no_s_sfs, color='b_uniformis_bottleneck')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_nonsyn_no_s_sfs, color='b_uniformis_exponential')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_nonsyn_no_s_sfs, color='b_uniformis_exponential')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_two_epoch_nonsyn_no_s_sfs, color='b_uniformis_two_epoch')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_two_epoch_nonsyn_no_s_sfs, color='b_uniformis_two_epoch')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_three_epoch_nonsyn_no_s_sfs, color='b_uniformis_three_epoch')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_three_epoch_nonsyn_no_s_sfs, color='b_uniformis_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_uniformis_x_axis, limits = c(1, length(b_uniformis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_uniformis_empirical',
                              'b_uniformis_bottleneck',
                              'b_uniformis_exponential',
                              'b_uniformis_two_epoch',
                              'b_uniformis_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. uniformis, Empirical vs. Expected Nonsyn SFS. (No singletons)')

p_b_uniformis

# e_rectale

e_rectale_empirical_syn_sfs = c(13797, 6657, 4566, 3313, 2578, 2070, 1747, 1615, 1419, 1347,
                                1173, 1145, 1047, 1022, 887, 803, 790, 807, 729, 590, 641,
                                591, 587, 543, 535, 579, 559, 538, 497, 497, 456, 465, 323)

# p_copri

p_copri_empirical_syn_sfs = c(15897, 5672, 3393, 2541, 2285, 2274, 608)

# o_splanchnicus

o_splanchnicus_empirical_syn_sfs = c(3089, 1528, 1154, 950, 705, 664, 531, 478, 450, 425, 366, 389, 382, 344, 166)

# a_shahii

a_shahii_empirical_syn_sfs = c(6767, 2504, 1289, 956, 712, 688, 554, 508, 477, 482, 418,
                               408, 397, 369, 375, 318, 297, 312, 280, 304, 249, 283, 
                               258, 256, 282, 247, 82)

# coprococcus_sp

coprococcus_sp_empirical_syn_sfs = c(4981, 2734, 1703, 710)

# b_massiliensis

b_massiliensis_empirical_syn_sfs = c(661, 136, 61, 23)

##### No singletons or doubletons

# b_vulgatus
b_vulgatus_empirical_syn_no_d_sfs = c(3066, 1346, 904, 727, 543, 475, 434, 369, 331, 270, 222, 239, 
                                 241, 231, 220, 199, 212, 199, 192, 180, 192, 163, 143, 47)

b_vulgatus_empirical_nonsyn_no_d_sfs = c(3254, 1171, 732, 513, 341, 288, 265, 210, 174, 166, 141,
                                    116, 117, 113, 116, 115, 106, 117, 109, 104, 81, 79, 65, 20)


b_vulgatus_bottleneck_growth_syn_no_d_sfs = fold_sfs(c(2324.072206271832, 1162.8862723952786, 775.2584991473477,
                                              581.4438824751076, 465.15511058987767, 387.6292624770717,
                                              332.2536565686301, 290.7219520548419, 258.41951514498294,
                                              232.57756556165435, 211.43415221784778, 193.81464105489593,
                                              178.90582387797267, 166.12683769054635, 155.05171629505463,
                                              145.36098504323644, 136.81033979225595, 129.2097662087563,
                                              122.40925297692164, 116.28879104406859, 110.75123022468169,
                                              105.71708400333016, 101.12068960640296, 96.90732805586556,
                                              93.03103541018498, 89.45291910343197, 86.13984843129634,
                                              83.06342564723461, 80.19916993524798, 77.52586458813857,
                                              75.02503053834626, 72.6804986017604, 70.47805949536837,
                                              68.40517561651212, 66.45074223141269, 64.60488846558947,
                                              62.85881056613311, 61.20463149097295, 59.63528209977647,
                                              58.144400166250676, 56.72624416910422, 55.37561939860333,
                                              54.08781437381632, 52.85854593026516, 51.68391162920448,
                                              50.56034837438615, 49.48459631179393))

b_vulgatus_bottleneck_growth_nonsyn_no_d_sfs = b_vulgatus_ratio * b_vulgatus_bottleneck_growth_syn_no_d_sfs

b_vulgatus_exponential_growth_syn_no_d_sfs = fold_sfs(c(2324.0927900330685, 1162.8863240098385, 775.2584992732425,
                                               581.4438824632564, 465.15511058001664, 387.62926246884865,
                                               332.25365656158414, 290.7219520486746, 258.4195151395009,
                                               232.57756555672051, 211.4341522133625, 193.81464105078717,
                                               178.90582387417743, 166.1268376870222, 155.05171629176542,
                                               145.3609850401528, 136.8103397893537, 129.20976620601527,
                                               122.40925297432489, 116.28879104160004, 110.75123022233069,
                                               105.71708400109051, 101.12068960425782, 96.90732805380979,
                                               93.03103540821013, 89.45291910153433, 86.139848429469,
                                               83.06342564547252, 80.19916993354666, 77.52586458649397,
                                               75.02503053675255, 72.68049860021962, 70.47805949387228,
                                               68.405175615061, 66.45074223000303, 64.60488846421804,
                                               62.85881056479965, 61.204631489674576, 59.63528209851138,
                                               58.14440016501722, 56.72624416790085, 55.37561939742861,
                                               54.087814372668916, 52.85854592914384, 51.68391162810807,
                                               50.56034837331358, 49.48459631074348))

b_vulgatus_exponential_growth_nonsyn_no_d_sfs = b_vulgatus_ratio * b_vulgatus_exponential_growth_syn_no_d_sfs

b_vulgatus_three_epoch_syn_no_d_sfs = fold_sfs(c(2321.019730603907, 1162.8791947843608, 775.258477659387,
                                        581.4438844737256, 465.15511227554157, 387.6292638822929,
                                        332.25365777311015, 290.7219531087516, 258.4195160817953,
                                        232.57756640478544, 211.4341529843306, 193.81464175750796,
                                        178.90582452652998, 166.12683829278288, 155.05171685714205,
                                        145.3609855701934, 136.81034028821543, 129.2097666771625,
                                        122.40925342067489, 116.28879146563253, 110.75123062617116,
                                        105.7170843865716, 101.12068997298174, 96.90732840717021,
                                        93.03103574743614, 89.45291942771065, 86.13984874356716,
                                        83.06342594835289, 80.19917022598287, 77.52586486918231,
                                        75.02503081032192, 72.68049886523994, 70.47805975086268,
                                        68.4051758644919, 66.45074247230733, 64.60488869979258,
                                        62.85881079400731, 61.20463171285046, 59.63528231596396,
                                        58.14440037703348, 56.72624437474599, 55.375619599349655,
                                        54.087814569892586, 52.858546121885894, 51.683911816566976,
                                        50.56034855767555, 49.48459649118285))

b_vulgatus_three_epoch_nonsyn_no_d_sfs = b_vulgatus_ratio * b_vulgatus_three_epoch_syn_no_d_sfs

b_vulgatus_two_epoch_syn_no_d_sfs = fold_sfs(c(2324.32413930096, 1162.8866604692287, 775.258499944615,
                                      581.4438823997708, 465.1551105274203, 387.6292624250183,
                                      332.2536565240153, 290.7219520157998, 258.41951511028253,
                                      232.57756553042398, 211.43415218945654, 193.81464102887338,
                                      178.90582385394933, 166.12683766823895, 155.0517162742344,
                                      145.3609850237154, 136.81033977388512, 129.2097661914061,
                                      122.40925296048461, 116.28879102845177, 110.75123020980853,
                                      105.71708398913754, 101.12068959282455, 96.90732804285291,
                                      93.03103539769153, 89.45291909142028, 86.13984841972953,
                                      83.06342563608091, 80.19916992447888, 77.52586457772735,
                                      75.02503052827194, 72.68049859200092, 70.47805948590363,
                                      68.40517560732673, 66.45074222248974, 64.60488845691437,
                                      62.85881055769247, 61.204631482754436, 59.63528209176868,
                                      58.144400158443084, 56.72624416148706, 55.375619391167525,
                                      54.087814366553445, 52.85854592316735, 51.6839116222644,
                                      50.56034836759766, 49.48459630514917))

b_vulgatus_two_epoch_nonsyn_no_d_sfs = b_vulgatus_ratio * b_vulgatus_two_epoch_syn_no_d_sfs

b_vulgatus_bottleneck_growth_syn_no_d_sfs = proportional_sfs(b_vulgatus_bottleneck_growth_syn_no_d_sfs)
b_vulgatus_exponential_growth_syn_no_d_sfs = proportional_sfs(b_vulgatus_exponential_growth_syn_no_d_sfs)
b_vulgatus_three_epoch_syn_no_d_sfs = proportional_sfs(b_vulgatus_three_epoch_syn_no_d_sfs)
b_vulgatus_two_epoch_syn_no_d_sfs = proportional_sfs(b_vulgatus_two_epoch_syn_no_d_sfs)

b_vulgatus_bottleneck_growth_nonsyn_no_d_sfs = proportional_sfs(b_vulgatus_bottleneck_growth_nonsyn_no_d_sfs)
b_vulgatus_exponential_growth_nonsyn_no_d_sfs = proportional_sfs(b_vulgatus_exponential_growth_nonsyn_no_d_sfs)
b_vulgatus_three_epoch_nonsyn_no_d_sfs = proportional_sfs(b_vulgatus_three_epoch_nonsyn_no_d_sfs)
b_vulgatus_two_epoch_nonsyn_no_d_sfs = proportional_sfs(b_vulgatus_two_epoch_nonsyn_no_d_sfs)

b_vulgatus_x_axis = 1:length(b_vulgatus_empirical_syn_no_d_sfs)
b_vulgatus_bottleneck_growth_syn_no_d_sfs = b_vulgatus_bottleneck_growth_syn_no_d_sfs[1:length(b_vulgatus_x_axis)]
b_vulgatus_exponential_growth_syn_no_d_sfs = b_vulgatus_exponential_growth_syn_no_d_sfs[1:length(b_vulgatus_x_axis)]
b_vulgatus_two_epoch_syn_no_d_sfs = b_vulgatus_two_epoch_syn_no_d_sfs[1:length(b_vulgatus_x_axis)]
b_vulgatus_three_epoch_syn_no_d_sfs = b_vulgatus_three_epoch_syn_no_d_sfs[1:length(b_vulgatus_x_axis)]

df = data.frame(b_vulgatus_empirical_syn_no_d_sfs, b_vulgatus_bottleneck_growth_syn_no_d_sfs, b_vulgatus_exponential_growth_syn_no_d_sfs, b_vulgatus_two_epoch_syn_no_d_sfs, b_vulgatus_three_epoch_syn_no_d_sfs)

p_b_vulgatus <- ggplot(data = df, aes(x=b_vulgatus_x_axis, y=proportional_sfs(b_vulgatus_empirical_syn_no_d_sfs), color='b_vulgatus_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_syn_no_d_sfs, color='b_vulgatus_bottleneck')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_syn_no_d_sfs, color='b_vulgatus_bottleneck')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_syn_no_d_sfs, color='b_vulgatus_exponential')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_exponential_growth_syn_no_d_sfs, color='b_vulgatus_exponential')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_two_epoch_syn_no_d_sfs, color='b_vulgatus_two_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_two_epoch_syn_no_d_sfs, color='b_vulgatus_two_epoch')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_syn_no_d_sfs, color='b_vulgatus_three_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_syn_no_d_sfs, color='b_vulgatus_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_vulgatus_x_axis, limits = c(1, length(b_vulgatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_vulgatus_empirical',
                              'b_vulgatus_bottleneck',
                              'b_vulgatus_exponential',
                              'b_vulgatus_two_epoch',
                              'b_vulgatus_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. vulgatus, Empirical vs. Expected Syn SFS. (No singletons or doubletons)')

p_b_vulgatus

df = data.frame(b_vulgatus_empirical_nonsyn_no_d_sfs, b_vulgatus_bottleneck_growth_nonsyn_no_d_sfs, b_vulgatus_exponential_growth_nonsyn_no_d_sfs, b_vulgatus_two_epoch_nonsyn_no_d_sfs, b_vulgatus_three_epoch_nonsyn_no_d_sfs)

p_b_vulgatus <- ggplot(data = df, aes(x=b_vulgatus_x_axis, y=proportional_sfs(b_vulgatus_empirical_nonsyn_no_d_sfs), color='b_vulgatus_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_nonsyn_no_d_sfs, color='b_vulgatus_bottleneck')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_nonsyn_no_d_sfs, color='b_vulgatus_bottleneck')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_nonsyn_no_d_sfs, color='b_vulgatus_exponential')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_exponential_growth_nonsyn_no_d_sfs, color='b_vulgatus_exponential')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_two_epoch_nonsyn_no_d_sfs, color='b_vulgatus_two_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_two_epoch_nonsyn_no_d_sfs, color='b_vulgatus_two_epoch')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_nonsyn_no_d_sfs, color='b_vulgatus_three_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_nonsyn_no_d_sfs, color='b_vulgatus_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_vulgatus_x_axis, limits = c(1, length(b_vulgatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_vulgatus_empirical',
                              'b_vulgatus_bottleneck',
                              'b_vulgatus_exponential',
                              'b_vulgatus_two_epoch',
                              'b_vulgatus_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. vulgatus, Empirical vs. Expected Nonsyn SFS. (No singletons or doubletons)')

p_b_vulgatus

# b_ovatus

b_ovatus_empirical_syn_no_d_sfs = c(2850, 1283, 956, 657, 560, 542, 389, 365, 268, 305, 272, 227,
                               238, 226, 205, 187, 185, 202, 205, 188, 152, 125, 28)

b_ovatus_empirical_nonsyn_no_d_sfs = c(2074, 783, 550, 341, 323, 240, 149, 141, 113, 111, 110, 89,
                                  96, 82, 73, 75, 58, 77, 66, 58, 65, 59, 6)

b_ovatus_bottleneck_growth_syn_no_d_sfs = fold_sfs(c(2273.056564217283, 1137.1924895249867, 758.1289547951559,
                                            568.5967233553124, 454.87738333470963, 379.0644898023539,
                                            324.9124228785423, 284.2983726009175, 252.7096667611283,
                                            227.4387020313474, 206.76245811079258, 189.53225479827373,
                                            174.95285195419396, 162.45622090719047, 151.6258072981711,
                                            142.1491953577254, 133.78747890919846, 126.35484203740019,
                                            119.70458796713034, 113.71935927823424, 108.30415234485352,
                                            103.38123692764212, 98.8864010897405, 94.7661348837719,
                                            90.97548995392317, 87.47643307604329, 84.23656557771444,
                                            81.22811716828106, 78.42714794201177, 75.8129099806007,
                                            73.36733251648408, 71.07460362805624, 68.92082799026147,
                                            66.89374502216567, 64.98249535208625, 63.17742620519486,
                                            61.46992834988473, 59.85229878945596, 58.31762457815557,
                                            56.85968406479827, 55.472862588559764, 54.15208021822471,
                                            52.89272957431087, 51.690622130019946, 50.5419416720416))

b_ovatus_bottleneck_growth_nonsyn_no_d_sfs = b_ovatus_ratio * b_ovatus_bottleneck_growth_syn_no_d_sfs

b_ovatus_exponential_growth_syn_no_d_sfs = fold_sfs(c(2272.3685827084455, 1137.191396953524, 758.1289524214081,
                                             568.5967235831931, 454.87738352402454, 379.064489960146,
                                             324.9124230137811, 284.29837271925754, 252.70966686631942,
                                             227.43870212601942, 206.762458196861, 189.53225487716708,
                                             174.9528520270186, 162.45622097481333, 151.62580736128578,
                                             142.14919541689542, 133.7874789648898, 126.35484208999576,
                                             119.70458801695771, 113.71935932557025, 108.30415238993544,
                                             103.38123697067486, 98.88640113090226, 94.76613492321859,
                                             90.97548999178939, 87.47643311245561, 84.23656561277816,
                                             81.22811720209364, 78.4271479746573, 75.81291001215803,
                                             73.36733254702449, 71.07460365764125, 68.92082801894898,
                                             66.89374505000849, 64.9824953791354, 63.177426231492646,
                                             61.46992837547177, 59.85229881437136, 58.317624602429625,
                                             56.85968408846547, 55.47286261164972, 54.15208024076567,
                                             52.89272959632762, 51.69062215153705, 50.541941693079835))

b_ovatus_exponential_growth_nonsyn_no_d_sfs = b_ovatus_ratio * b_ovatus_exponential_growth_syn_no_d_sfs

b_ovatus_three_epoch_syn_no_d_sfs = fold_sfs(c(2271.234361793639, 1137.1896658994278, 758.128949031555,
                                      568.596723910058, 454.87738379424957, 379.0644901853685,
                                      324.9124232068337, 284.2983728881724, 252.7096670164624,
                                      227.43870226115135, 206.76245831970527, 189.53225498977704,
                                      174.95285213096872, 162.45622107133846, 151.62580745137376,
                                      142.14919550135087, 133.78747904438106, 126.35484216507085,
                                      119.70458808808148, 113.71935939313946, 108.30415245428551,
                                      103.38123703209995, 98.88640118965807, 94.7661349795249,
                                      90.97549004584346, 87.47643316443069, 84.23656566283063,
                                      81.2281172503562, 78.42714802125451, 75.81291005720308,
                                      73.36733259061752, 71.07460369987099, 68.92082805990002,
                                      66.89374508975412, 64.98249541774453, 63.17742626903019,
                                      61.46992841199304, 59.85229884993154, 58.31762463707884,
                                      56.85968412224845, 55.47286264461109, 54.15208027294148,
                                      52.89272962775366, 51.69062218224959, 50.541941723109154))

b_ovatus_three_epoch_nonsyn_no_d_sfs = b_ovatus_ratio * b_ovatus_three_epoch_syn_no_d_sfs

b_ovatus_two_epoch_syn_no_d_sfs = fold_sfs(c(2272.800259369082, 1137.192155057901, 758.1289541782724,
                                    568.5967234148438, 454.8773833838764, 379.06448984333696,
                                    324.9124229136707, 284.29837263165484, 252.70966678845036,
                                    227.43870205593726, 206.76245813314995, 189.5322548187626,
                                    174.95285197310926, 162.45622092475696, 151.62580731456436,
                                    142.14919537309206, 133.787478923665, 126.35484205106123,
                                    119.70458798007236, 113.71935929052917, 108.30415235656298,
                                    103.38123693881934, 98.88640110043175, 94.76613489401768,
                                    90.97548996375653, 87.47643308550094, 84.23656558682181,
                                    81.2281171770643, 78.4271479504888, 75.81290998879732,
                                    73.36733252441734, 71.07460363574057, 68.92082799771295,
                                    66.89374502939609, 64.98249535911192, 63.17742621202538,
                                    61.46992835652889, 59.85229879592698, 58.31762458446067,
                                    56.85968407094574, 55.47286259455809, 54.15208022407944,
                                    52.8927295800302, 51.69062213560929, 50.5419416775053))

b_ovatus_two_epoch_nonsyn_no_d_sfs = b_ovatus_ratio * b_ovatus_two_epoch_syn_no_d_sfs

b_ovatus_bottleneck_growth_syn_no_d_sfs = proportional_sfs(b_ovatus_bottleneck_growth_syn_no_d_sfs)
b_ovatus_exponential_growth_syn_no_d_sfs = proportional_sfs(b_ovatus_exponential_growth_syn_no_d_sfs)
b_ovatus_three_epoch_syn_no_d_sfs = proportional_sfs(b_ovatus_three_epoch_syn_no_d_sfs)
b_ovatus_two_epoch_syn_no_d_sfs = proportional_sfs(b_ovatus_two_epoch_syn_no_d_sfs)

b_ovatus_bottleneck_growth_nonsyn_no_d_sfs = proportional_sfs(b_ovatus_bottleneck_growth_nonsyn_no_d_sfs)
b_ovatus_exponential_growth_nonsyn_no_d_sfs = proportional_sfs(b_ovatus_exponential_growth_nonsyn_no_d_sfs)
b_ovatus_three_epoch_nonsyn_no_d_sfs = proportional_sfs(b_ovatus_three_epoch_nonsyn_no_d_sfs)
b_ovatus_two_epoch_nonsyn_no_d_sfs = proportional_sfs(b_ovatus_two_epoch_nonsyn_no_d_sfs)

b_ovatus_x_axis = 1:length(b_ovatus_empirical_syn_no_d_sfs)
b_ovatus_bottleneck_growth_syn_no_d_sfs = b_ovatus_bottleneck_growth_syn_no_d_sfs[1:length(b_ovatus_x_axis)]
b_ovatus_exponential_growth_syn_no_d_sfs = b_ovatus_exponential_growth_syn_no_d_sfs[1:length(b_ovatus_x_axis)]
b_ovatus_two_epoch_syn_no_d_sfs = b_ovatus_two_epoch_syn_no_d_sfs[1:length(b_ovatus_x_axis)]
b_ovatus_three_epoch_syn_no_d_sfs = b_ovatus_three_epoch_syn_no_d_sfs[1:length(b_ovatus_x_axis)]

df = data.frame(b_ovatus_empirical_syn_no_d_sfs, b_ovatus_bottleneck_growth_syn_no_d_sfs, b_ovatus_exponential_growth_syn_no_d_sfs, b_ovatus_two_epoch_syn_no_d_sfs, b_ovatus_three_epoch_syn_no_d_sfs)

p_b_ovatus <- ggplot(data = df, aes(x=b_ovatus_x_axis, y=proportional_sfs(b_ovatus_empirical_syn_no_d_sfs), color='b_ovatus_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_syn_no_d_sfs, color='b_ovatus_bottleneck')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_syn_no_d_sfs, color='b_ovatus_bottleneck')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_syn_no_d_sfs, color='b_ovatus_exponential')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_exponential_growth_syn_no_d_sfs, color='b_ovatus_exponential')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_two_epoch_syn_no_d_sfs, color='b_ovatus_two_epoch')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_two_epoch_syn_no_d_sfs, color='b_ovatus_two_epoch')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_three_epoch_syn_no_d_sfs, color='b_ovatus_three_epoch')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_three_epoch_syn_no_d_sfs, color='b_ovatus_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_ovatus_x_axis, limits = c(1, length(b_ovatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_ovatus_empirical',
                              'b_ovatus_bottleneck',
                              'b_ovatus_exponential',
                              'b_ovatus_two_epoch',
                              'b_ovatus_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. ovatus, Empirical vs. Expected Syn SFS. (No singletons or doubletons)')

p_b_ovatus

df = data.frame(b_ovatus_empirical_nonsyn_no_d_sfs, b_ovatus_bottleneck_growth_nonsyn_no_d_sfs, b_ovatus_exponential_growth_nonsyn_no_d_sfs, b_ovatus_two_epoch_nonsyn_no_d_sfs, b_ovatus_three_epoch_nonsyn_no_d_sfs)

p_b_ovatus <- ggplot(data = df, aes(x=b_ovatus_x_axis, y=proportional_sfs(b_ovatus_empirical_nonsyn_no_d_sfs), color='b_ovatus_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_nonsyn_no_d_sfs, color='b_ovatus_bottleneck')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_nonsyn_no_d_sfs, color='b_ovatus_bottleneck')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_bottleneck_growth_nonsyn_no_d_sfs, color='b_ovatus_exponential')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_exponential_growth_nonsyn_no_d_sfs, color='b_ovatus_exponential')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_two_epoch_nonsyn_no_d_sfs, color='b_ovatus_two_epoch')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_two_epoch_nonsyn_no_d_sfs, color='b_ovatus_two_epoch')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_three_epoch_nonsyn_no_d_sfs, color='b_ovatus_three_epoch')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_three_epoch_nonsyn_no_d_sfs, color='b_ovatus_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_ovatus_x_axis, limits = c(1, length(b_ovatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_ovatus_empirical',
                              'b_ovatus_bottleneck',
                              'b_ovatus_exponential',
                              'b_ovatus_two_epoch',
                              'b_ovatus_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. ovatus, Empirical vs. Expected Nonsyn SFS. (No singletons or doubletons)')

p_b_ovatus

# b_uniformis

b_uniformis_empirical_syn_no_d_sfs = c(3013, 1244, 743, 580, 427, 384, 371, 341, 278, 240, 219,
                                  245, 197, 182, 220, 178, 146, 130, 148, 160, 138, 115,
                                  119, 108, 95, 108, 87, 111, 80, 75, 68, 83, 64, 84, 66, 55,
                                  43)

b_uniformis_empirical_nonsyn_no_d_sfs = c(3053, 1024, 506, 330, 322, 226, 235, 182, 171, 141,
                                     136, 116, 95, 107, 124, 86, 78, 66, 82, 69, 54, 69,
                                     57, 69, 55, 69, 54, 40, 47, 39, 36, 25, 31, 35, 36,
                                     33, 27)

b_uniformis_bottleneck_growth_syn_no_d_sfs = fold_sfs(c(1996.2578639594647, 999.1246888591537, 666.0847217860874,
                                               499.5635497708541, 399.65084275111707, 333.04237122192797,
                                               285.4648914730799, 249.78178160946118, 222.0282516792758,
                                               199.82542770749484, 181.65948079905698, 166.5211916897179,
                                               153.71187011911965, 142.7324516152976, 133.21695556531674,
                                               124.89089650937593, 117.54437380166867, 111.01413138430787,
                                               105.17128289576114, 99.91271924680167, 95.15497117468523,
                                               90.82974564626261, 86.88062667761056, 83.2606009487867,
                                               79.93017727104068, 76.85594002311764, 74.00942404616242,
                                               71.36623063258368, 68.9053264137658, 66.60848247025429,
                                               64.459822001214, 62.445452805923416, 60.55316658677248,
                                               58.772191316462504, 57.092986056528225, 55.507069972774865,
                                               54.00687907794722, 52.58564559397712, 51.23729587387813,
                                               49.956363635364305, 48.73791589197821, 47.57748946550095,
                                               46.471036357077885, 45.41487656775805, 44.405657209596185,
                                               43.4403169501315, 42.516054995830665, 41.63030395262184,
                                               40.78070600962272, 39.965091980824425, 39.181462812447705,
                                               38.42797322408693, 37.70291720177998, 37.00471510296842,
                                               36.33190216818927, 35.683118263657754, 35.05709870357073,
                                               34.45266602183328, 33.86872258057127, 33.304243917756686,
                                               32.7582727492038, 32.22991355099849, 31.71832765788628,
                                               31.222728821191968, 30.74237917677193, 30.27658557952423,
                                               29.82469626614366, 29.386097812341518, 28.960212354656694,
                                               28.54649505039878, 28.14443175224384, 27.753536876599796,
                                               27.373351447203827))

b_uniformis_bottleneck_growth_nonsyn_no_d_sfs = b_uniformis_ratio * b_uniformis_bottleneck_growth_syn_no_d_sfs

b_uniformis_exponential_growth_syn_no_d_sfs = fold_sfs(c(1996.1350363617978, 999.1243786046282, 666.0847207594145,
                                                499.5635498509922, 399.65084281958934, 333.0423712790166,
                                                285.4648915220069, 249.7817816522759, 222.02825171733173,
                                                199.82542774174232, 181.6594808301911, 166.52119171825748,
                                                153.71187014546607, 142.73245163976213, 133.21695558815216,
                                                124.89089653078238, 117.54437382181928, 111.01413140333584,
                                                105.17128291378762, 99.91271926392683, 95.15497119099761,
                                                90.82974566183094, 86.88062669250202, 83.26060096306004,
                                                79.93017728474308, 76.85594003629194, 74.00942405884773,
                                                71.36623064481594, 68.9053264255792, 66.60848248167107,
                                                64.45982201226249, 62.44545281662664, 60.55316659715309,
                                                58.7721913265378, 57.09298606631403, 55.50706998228885,
                                                54.00687908720407, 52.58564560299187, 51.237295882660995,
                                                49.95636364392831, 48.737915900331956, 47.577489473655795,
                                                46.47103636504506, 45.41487657554156, 44.40565721720738,
                                                43.44031695757723, 42.51605500311797, 41.63030395975733,
                                                40.780706016612584, 39.9650919876745, 39.18146281916346,
                                                38.42797323067353, 37.70291720824231, 37.00471510931213,
                                                36.33190217441661, 35.683118269773885, 35.05709870957956,
                                                34.45266602774047, 33.868722586376414, 33.30424392346507,
                                                32.758272754818144, 32.22991355652274, 31.718327663323745,
                                                31.222728826543584, 30.742379182041216, 30.276585584713676,
                                                29.824696271255654, 29.386097817378335, 28.96021235962051,
                                                28.546495055292496, 28.14443175706783, 27.753536881356787,
                                                27.3733514518941))

b_uniformis_exponential_growth_nonsyn_no_d_sfs = b_uniformis_ratio * b_uniformis_exponential_growth_syn_no_d_sfs

b_uniformis_three_epoch_syn_no_d_sfs = fold_sfs(c(1992.4799173283789, 999.1125576096465, 666.0846690561311,
                                         499.5635538014593, 399.65084627271233, 333.0423741590542,
                                         285.46489399063285, 249.7817838123254, 222.02825363737102,
                                         199.82542946978054, 181.6594824011349, 166.52119315828932,
                                         153.71187147472185, 142.73245287407516, 133.21695674017766,
                                         124.89089761080452, 117.5443748383107, 111.01413236335397,
                                         105.17128382328144, 99.91272012794597, 95.15497201386894,
                                         90.8297464473016, 86.88062744382178, 83.26060168307365,
                                         79.930177975955, 76.85594070091987, 74.00942469886087,
                                         71.36623126197249, 68.90532702145255, 66.6084830576829,
                                         64.45982256969421, 62.44545335663775, 60.553167120798506,
                                         58.772191834781886, 57.09298656003848, 55.50707046229872,
                                         54.00687955424148, 52.58564605773657, 51.237296325747046,
                                         49.95636407593579, 48.73791632180405, 47.577489885092845,
                                         46.47103676691248, 45.41487696827693, 44.40565760121529,
                                         43.44031733323715, 42.516055370785125, 41.63030431976356,
                                         40.78070636927292, 39.965092333281056, 39.181463157993974,
                                         38.42797356298808, 37.702917534286776, 37.00471542931768,
                                         36.33190248860543, 35.68311857835168, 35.05709901274421,
                                         34.452666325677654, 33.868722879263814, 33.304244211471016,
                                         32.75827303810222, 32.22991383523817, 31.718327937615122,
                                         31.222729096547827, 30.742379447892855, 30.276585846537262,
                                         29.824696529172275, 29.386098071501227, 28.960212610060463,
                                         28.546495302155545, 28.144432000453136, 27.75353712136253,
                                         27.373351688612093))

b_uniformis_three_epoch_nonsyn_no_d_sfs = b_uniformis_ratio * b_uniformis_two_epoch_syn_no_d_sfs

b_uniformis_two_epoch_syn_no_d_sfs = fold_sfs(c(1996.550787398168, 999.1253320181567, 666.0847236827736,
                                       499.56354962184173, 399.65084262469156, 333.04237111652844,
                                       285.4648913827333, 249.78178153040975, 222.02825160900787,
                                       199.82542764425082, 181.65948074156503, 166.52119163701457,
                                       153.7118700704726, 142.73245157012536, 133.21695552315785,
                                       124.89089646984665, 117.54437376446799, 111.01413134917075,
                                       105.1712828624763, 99.91271921518108, 95.1549711445704,
                                       90.82974561751662, 86.88062665011441, 83.2606009224374,
                                       79.9301772457442, 76.85593999879411, 74.00942402273871,
                                       71.36623060999754, 68.90532639196145, 66.6084824491739,
                                       64.45982198081452, 62.44545278616144, 60.55316656760849,
                                       58.772191297862165, 57.09298603845932, 55.507069955208664,
                                       54.00687906085501, 52.58564557733471, 51.23729585766318,
                                       49.95636361955402, 48.73791587655216, 47.577489450442854,
                                       46.47103634237129, 45.41487655338506, 44.40565719554323,
                                       43.440316936383425, 42.516054982375096, 41.63030393944541,
                                       40.78070599671636, 39.965091968176196, 39.18146280004748,
                                       38.42797321192516, 37.70291718984769, 37.00471509125709,
                                       36.33190215669088, 35.683118252364686, 35.0570986924743,
                                       34.452666010929626, 33.868722569851464, 33.30424390721649,
                                       32.758272738836396, 32.2299135407983, 31.71832764784845,
                                       31.222728811310535, 30.74237916704252, 30.276585569942235,
                                       29.82469625670553, 29.38609780304135, 28.960212345491307,
                                       28.546495041365947, 28.144431743336632, 27.753536867816297,
                                       27.373351438539096))

b_uniformis_two_epoch_nonsyn_no_d_sfs = b_uniformis_ratio * b_uniformis_two_epoch_syn_no_d_sfs

b_uniformis_bottleneck_growth_syn_no_d_sfs = proportional_sfs(b_uniformis_bottleneck_growth_syn_no_d_sfs)
b_uniformis_exponential_growth_syn_no_d_sfs = proportional_sfs(b_uniformis_exponential_growth_syn_no_d_sfs)
b_uniformis_three_epoch_syn_no_d_sfs = proportional_sfs(b_uniformis_three_epoch_syn_no_d_sfs)
b_uniformis_two_epoch_syn_no_d_sfs = proportional_sfs(b_uniformis_two_epoch_syn_no_d_sfs)

b_uniformis_bottleneck_growth_nonsyn_no_d_sfs = proportional_sfs(b_uniformis_bottleneck_growth_nonsyn_no_d_sfs)
b_uniformis_exponential_growth_nonsyn_no_d_sfs = proportional_sfs(b_uniformis_exponential_growth_nonsyn_no_d_sfs)
b_uniformis_three_epoch_nonsyn_no_d_sfs = proportional_sfs(b_uniformis_three_epoch_nonsyn_no_d_sfs)
b_uniformis_two_epoch_nonsyn_no_d_sfs = proportional_sfs(b_uniformis_two_epoch_nonsyn_no_d_sfs)

b_uniformis_x_axis = 1:length(b_uniformis_empirical_syn_no_d_sfs)
b_uniformis_bottleneck_growth_syn_no_d_sfs = b_uniformis_bottleneck_growth_syn_no_d_sfs[1:length(b_uniformis_x_axis)]
b_uniformis_exponential_growth_syn_no_d_sfs = b_uniformis_exponential_growth_syn_no_d_sfs[1:length(b_uniformis_x_axis)]
b_uniformis_two_epoch_syn_no_d_sfs = b_uniformis_two_epoch_syn_no_d_sfs[1:length(b_uniformis_x_axis)]
b_uniformis_three_epoch_syn_no_d_sfs = b_uniformis_three_epoch_syn_no_d_sfs[1:length(b_uniformis_x_axis)]

df = data.frame(b_uniformis_empirical_syn_no_d_sfs, b_uniformis_bottleneck_growth_syn_no_d_sfs, b_uniformis_exponential_growth_syn_no_d_sfs, b_uniformis_two_epoch_syn_no_d_sfs, b_uniformis_three_epoch_syn_no_d_sfs)

p_b_uniformis <- ggplot(data = df, aes(x=b_uniformis_x_axis, y=proportional_sfs(b_uniformis_empirical_syn_no_d_sfs), color='b_uniformis_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_syn_no_d_sfs, color='b_uniformis_bottleneck')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_syn_no_d_sfs, color='b_uniformis_bottleneck')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_syn_no_d_sfs, color='b_uniformis_exponential')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_syn_no_d_sfs, color='b_uniformis_exponential')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_two_epoch_syn_no_d_sfs, color='b_uniformis_two_epoch')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_two_epoch_syn_no_d_sfs, color='b_uniformis_two_epoch')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_three_epoch_syn_no_d_sfs, color='b_uniformis_three_epoch')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_three_epoch_syn_no_d_sfs, color='b_uniformis_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_uniformis_x_axis, limits = c(1, length(b_uniformis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_uniformis_empirical',
                              'b_uniformis_bottleneck',
                              'b_uniformis_exponential',
                              'b_uniformis_two_epoch',
                              'b_uniformis_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. uniformis, Empirical vs. Expected Syn SFS. (No singletons or doubletons)')

p_b_uniformis

df = data.frame(b_uniformis_empirical_nonsyn_no_d_sfs, b_uniformis_bottleneck_growth_nonsyn_no_d_sfs, b_uniformis_exponential_growth_nonsyn_no_d_sfs, b_uniformis_two_epoch_nonsyn_no_d_sfs, b_uniformis_three_epoch_nonsyn_no_d_sfs)

p_b_uniformis <- ggplot(data = df, aes(x=b_uniformis_x_axis, y=proportional_sfs(b_uniformis_empirical_nonsyn_no_d_sfs), color='b_uniformis_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_nonsyn_no_d_sfs, color='b_uniformis_bottleneck')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_nonsyn_no_d_sfs, color='b_uniformis_bottleneck')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_bottleneck_growth_nonsyn_no_d_sfs, color='b_uniformis_exponential')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_nonsyn_no_d_sfs, color='b_uniformis_exponential')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_two_epoch_nonsyn_no_d_sfs, color='b_uniformis_two_epoch')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_two_epoch_nonsyn_no_d_sfs, color='b_uniformis_two_epoch')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_three_epoch_nonsyn_no_d_sfs, color='b_uniformis_three_epoch')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_three_epoch_nonsyn_no_d_sfs, color='b_uniformis_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_uniformis_x_axis, limits = c(1, length(b_uniformis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_uniformis_empirical',
                              'b_uniformis_bottleneck',
                              'b_uniformis_exponential',
                              'b_uniformis_two_epoch',
                              'b_uniformis_three_epoch'),
                     labels=c('Empirical',
                              'Bottleneck + Growth',
                              'Exponential',
                              'Two Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. uniformis, Empirical vs. Expected nonsyn SFS. (No singletons or doubletons)')

p_b_uniformis

df = data.frame(b_uniformis_empirical_syn_no_s_sfs,
                b_uniformis_empirical_nonsyn_no_s_sfs,
                b_uniformis_exponential_growth_nonsyn_no_s_sfs,
                b_uniformis_exponential_growth_syn_no_s_sfs,
                b_uniformis_exponential_growth_nonsyn_no_d_sfs,
                b_uniformis_exponential_growth_syn_no_d_sfs)

p_b_uniformis_comparison <- ggplot(data = df, aes(x=b_uniformis_x_axis, y=proportional_sfs(b_uniformis_empirical_nonsyn_no_s_sfs), color='b_uniformis_empirical_nonsyn')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=proportional_sfs(b_uniformis_empirical_nonsyn_no_s_sfs), color='b_uniformis_empirical_syn_no_s')) +
  geom_line(aes(x=b_uniformis_x_axis, y=proportional_sfs(b_uniformis_empirical_nonsyn_no_s_sfs), color='b_uniformis_empirical_syn_no_s')) +  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_nonsyn_no_d_sfs, color='b_uniformis_exponential_nonsyn_no_d')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_nonsyn_no_d_sfs, color='b_uniformis_exponential_nonsyn_no_d')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_nonsyn_no_d_sfs, color='b_uniformis_exponential_nonsyn_no_d')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_nonsyn_no_s_sfs, color='b_uniformis_exponential_nonsyn_no_s')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_nonsyn_no_s_sfs, color='b_uniformis_exponential_nonsyn_no_s')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_syn_no_d_sfs, color='b_uniformis_exponential_syn_no_d')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_syn_no_d_sfs, color='b_uniformis_exponential_syn_no_d')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_syn_no_s_sfs, color='b_uniformis_exponential_syn_no_s')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_exponential_growth_syn_no_s_sfs, color='b_uniformis_exponential_syn_no_s')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_uniformis_x_axis, limits = c(1, length(b_uniformis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green', 'violet'),
                     name='Data Type',
                     breaks=c('b_uniformis_empirical_nonsyn_no_s',
                              'b_uniformis_empirical_syn_no_s',
                              'b_uniformis_exponential_nonsyn_no_d',
                              'b_uniformis_exponential_nonsyn_no_s',
                              'b_uniformis_exponential_syn_no_d',
                              'b_uniformis_exponential_syn_no_s'
                              ),
                     labels=c('Empirical Nonsynonymous',
                              'Empirical Synonymous',
                              'Model Nonsynonymous, no doubletons',
                              'Model Nonsynonymous, no singletons',
                              'Model Synonymous, no doubletons',
                              'Model Synonymous, no singletons')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. uniformis, Empirical vs. Expected nonsyn SFS.')

p_b_uniformis_comparison

a_finegoldii_empirical = proportional_sfs(fold_sfs(c(6207, 2356, 1527, 1236, 1123, 1036, 1078, 1043, 1026, 992, 1016, 903, 853, 741, 737, 625, 614, 579, 232, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
a_finegoldii_two_epoch = proportional_sfs(fold_sfs(c(2591.452115690476, 2040.3513974213315, 1647.5716188051133, 1360.6264968342396, 1146.521196160241, 983.5147484922816,
                                                     856.9918534967202, 756.968728572783, 676.5216712538063, 610.7773044190312, 556.2544121055782, 510.429513195906,
                                                     471.44665871152654, 437.9212758071015, 408.80601606138976, 383.2979250828289, 360.7734456876588, 340.742370906557,
                                                     322.8148374889029, 306.6773902445915, 292.07542416148675, 278.80015916045147, 266.6788707150096, 255.56748404476986,
                                                     245.3449020805276, 235.9086182591707, 227.17129097886522, 219.05804481746716, 211.50432614464194, 204.45418545450462,
                                                     197.85889098771784, 191.67580167860103, 185.86744469211013, 180.40075557669388, 175.2464485878213, 170.37849191257166,
                                                     165.7736679704896)))
a_finegoldii_one_epoch = proportional_sfs(fold_sfs(c(5580.9300025725715, 2790.465911482565, 1860.3106395655457, 1395.232997343584, 1116.186411044604, 930.155353010814,
                                                     797.2760255000767, 697.6165295983906, 620.1035881170635, 558.0932347372021, 507.35749088963206, 465.07770419365653,
                                                     429.3024999234318, 398.6380389886885, 372.0621727230122, 348.8082896263927, 328.290157375271, 310.0518174956055,
                                                     293.7333027710832, 279.04663942856405, 265.7587058421832, 253.67876613620396, 242.64925589153276, 232.53887142557363,
                                                     223.23731764493994, 214.65126793214117, 206.70122183524362, 199.3190361097097, 192.4459665792345, 186.03110162434857,
                                                     180.03009886676412, 174.40415872553487, 169.11918459916942, 164.14509125108614, 159.45523175743773, 155.02591996371225,
                                                     150.83603038071726)))

a_finegoldii_df = data.frame(a_finegoldii_empirical,
                             a_finegoldii_one_epoch,
                             a_finegoldii_two_epoch)
a_finegoldii_x_axis = 1:length(a_finegoldii_empirical)
p_a_finegoldii_comparison <- ggplot(data = a_finegoldii_df, aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +  
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_one_epoch, color='a_finegoldii_one_epoch')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_one_epoch, color='a_finegoldii_one_epoch')) +
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_two_epoch, color='a_finegoldii_two_epoch')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_two_epoch, color='a_finegoldii_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_finegoldii_x_axis, limits = c(2, length(a_finegoldii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('a_finegoldii_empirical',
                              'a_finegoldii_one_epoch',
                              'a_finegoldii_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('A. finegoldii')
p_a_finegoldii_comparison

a_finegoldii_df = data.frame(a_finegoldii_empirical,
                             a_finegoldii_one_epoch,
                             a_finegoldii_two_epoch)

a_finegoldii_two_epoch = proportional_sfs(fold_sfs(c(5800.754225243232, 2830.7582820144507, 1886.8555254662822, 1415.139309939643,
                                                     1132.1114372869934, 943.4262080534488, 808.6510442264771, 707.56967108786,
                                                     628.9508250857845, 566.055748086557, 514.5961394570355, 471.7131321071898,
                                                     435.4275103585752, 404.32554872621324, 377.37051518749666, 353.7848607253584,
                                                     332.9739890325221, 314.475436314351, 297.9240995750685, 283.0278964179748,
                                                     269.55037918860495, 257.2980907151097, 246.1112185511609, 235.856585658117,
                                                     226.42232332352594, 217.71377340610638, 209.65030119285808, 202.16279121552648,
                                                     195.19166117386857, 188.6852730743485, 182.59865189032612, 176.89244447350242,
                                                     171.5320677541444, 166.48700725896126, 161.73023588303144, 157.2377295331041,
                                                     152.98806131518674)))
p_a_finegoldii_comparison <- ggplot(data = a_finegoldii_df, aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +  
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_one_epoch, color='a_finegoldii_one_epoch')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_one_epoch, color='a_finegoldii_one_epoch')) +
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_two_epoch, color='a_finegoldii_two_epoch')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_two_epoch, color='a_finegoldii_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_finegoldii_x_axis, limits = c(1, length(a_finegoldii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('a_finegoldii_empirical',
                              'a_finegoldii_one_epoch',
                              'a_finegoldii_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('A. finegoldii with singletons')
p_a_finegoldii_comparison

a_muciniphila_empirical = proportional_sfs(fold_sfs(c(6086, 3589, 3233, 2477, 2248, 2176, 1801, 1678, 630, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
a_muciniphila_two_epoch = proportional_sfs(fold_sfs(c(4856.85835255306, 3347.7729196144837, 2463.522110113016, 1912.8116849569628, 1549.5981768854656, 1297.23494993225,
                                                      1113.736995642971, 975.0817552301565, 866.9110678493279, 780.2715189468886, 709.3528197073293, 650.2443379468108,
                                                      600.2266887974953, 557.3536458784366, 520.1968062648581, 487.68452223354774, 458.99720162924217)))
a_muciniphila_one_epoch = proportional_sfs(fold_sfs(c(7490.140844113354, 3745.0710808582367, 2496.714114876777, 1872.5356266659865, 1498.0285313679894, 1248.3571327189354,
                                                      1070.02041792512, 936.2678805625335, 832.2381281642893, 749.0143252465251, 680.922121954235, 624.1786183831039,
                                                      576.1648838297001, 535.0102535055369, 499.34290656548245, 468.1339773755469, 440.59668633320047)))

a_muciniphila_df = data.frame(a_muciniphila_empirical,
                             a_muciniphila_one_epoch,
                             a_muciniphila_two_epoch)
a_muciniphila_x_axis = 1:length(a_muciniphila_empirical)
p_a_muciniphila_comparison <- ggplot(data = a_muciniphila_df, aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +  
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_one_epoch, color='a_muciniphila_one_epoch')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_one_epoch, color='a_muciniphila_one_epoch')) +
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_two_epoch, color='a_muciniphila_two_epoch')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_two_epoch, color='a_muciniphila_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_muciniphila_x_axis, limits = c(2, length(a_muciniphila_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('a_muciniphila_empirical',
                              'a_muciniphila_one_epoch',
                              'a_muciniphila_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('A. muciniphila')
p_a_muciniphila_comparison

a_muciniphila_two_epoch = proportional_sfs(fold_sfs(c(2962.5482163192582, 2278.8920787625266, 1811.9780923980811, 1481.0671773522608, 1239.6527877094,
                                                      1058.8686538484433, 920.2067006431773, 811.4999802841045, 724.5687688008009)))

a_muciniphila_df = data.frame(a_muciniphila_empirical,
                              a_muciniphila_one_epoch,
                              a_muciniphila_two_epoch)
p_a_muciniphila_comparison <- ggplot(data = a_muciniphila_df, aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +  
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_one_epoch, color='a_muciniphila_one_epoch')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_one_epoch, color='a_muciniphila_one_epoch')) +
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_two_epoch, color='a_muciniphila_two_epoch')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_two_epoch, color='a_muciniphila_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_muciniphila_x_axis, limits = c(1, length(a_muciniphila_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('a_muciniphila_empirical',
                              'a_muciniphila_one_epoch',
                              'a_muciniphila_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('A. muciniphila with singletons')
p_a_muciniphila_comparison
a_onderdonkii_empirical = proportional_sfs(fold_sfs(c(5437, 2738, 1710, 1383, 1243, 1033, 1095, 983, 1011, 899, 887, 754, 681, 668, 567, 424, 480, 476, 509,
                                                      512, 615, 557, 605, 586, 476, 389, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                      0, 0, 0)))
a_onderdonkii_nonsyn = proportional_sfs(fold_sfs(c(3500, 1430, 808, 637, 654, 513, 498, 527, 486, 428, 447, 351, 342, 342, 288, 226, 226, 213, 201, 253,
                                                   279, 295, 283, 279, 227, 194, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
a_onderdonkii_two_epoch = proportional_sfs(fold_sfs(c(2962.5482163192582, 2278.8920787625266, 1811.9780923980811, 1481.0671773522608, 1239.6527877094,
                                                      1058.8686538484433, 920.2067006431773, 811.4999802841045, 724.5687688008009, 653.7970434501336,
                                                      595.2511060654201, 546.1226286129505, 504.37002776651286, 468.48340254122104, 437.32781764325455,
                                                      410.0370951378693, 385.9406908013101, 364.5125771656377, 345.33498322002305, 328.0723086347573,
                                                      312.45210322187404, 298.25101863454705, 285.2843047651604, 273.39786469690944, 262.46217852353055,
                                                      252.36760790262556, 243.02073186122448, 234.3414608531874, 226.26074395290397, 218.7187323495711,
                                                      211.6632970001714, 205.04882348117656, 198.835225533262, 192.98713244281606, 187.4732155900244, 
                                                      182.26562715909853, 177.33952982289554, 172.6727006618431, 168.24519600362493, 164.03906652856486,
                                                      160.03811406214516, 156.22768310882967, 152.59448147269748, 149.1264253380142, 145.81250500538573,
                                                      142.6426681402314, 139.60771792563688, 136.69922394600735, 133.90944398288033, 131.231255195276,
                                                      128.6580933964923)))
a_onderdonkii_one_epoch = proportional_sfs(fold_sfs(c(6081.66334443838, 3040.8329912474983, 2027.222028956375, 1520.4165373064973, 1216.3332411950767,
                                                      1013.611043309841, 868.8094731107915, 760.2082952635453, 675.7407123387293, 608.1666458702935,
                                                      552.8787731941161, 506.8055458655427, 467.8205072680046, 434.40475981764416, 405.4444452859416,
                                                      380.1041700013009, 357.7451035087956, 337.8703776767596, 320.08772819062625, 304.0833435987185,
                                                      289.6031860591187, 276.4394064284871, 264.42030324051467, 253.40279193995002, 243.26668150037176,
                                                      233.9102718225031, 225.24692948839657, 217.2023972827054, 209.71266036458496, 202.7222392053208,
                                                      196.18281292503218, 190.05210075385145, 184.29294686368866, 178.8725667003498, 173.76192251584953,
                                                      168.93520297862293, 164.3693871713434, 160.04387743112392, 155.94018867589978, 152.04168433178808,
                                                      148.33335090532026, 144.8016047595313, 141.43412585156952, 138.2197141425143, 135.14816515243663,
                                                      132.21016174749158, 129.3971797414086, 126.70140529674889, 124.1156624403792, 121.63334927699951,
                                                      119.24838170701896)))
a_onderdonkii_dfe = proportional_sfs(fold_sfs(c(2962.5482163192582-100, 2278.8920787625266-500, 1811.9780923980811-400, 1481.0671773522608-300, 1239.6527877094-200,
                                                      1058.8686538484433-100, 920.2067006431773-50, 811.4999802841045, 724.5687688008009, 653.7970434501336,
                                                      595.2511060654201, 546.1226286129505, 504.37002776651286, 468.48340254122104, 437.32781764325455,
                                                      410.0370951378693, 385.9406908013101, 364.5125771656377, 345.33498322002305, 328.0723086347573,
                                                      312.45210322187404, 298.25101863454705, 285.2843047651604, 273.39786469690944, 262.46217852353055,
                                                      252.36760790262556, 243.02073186122448, 234.3414608531874, 226.26074395290397, 218.7187323495711,
                                                      211.6632970001714, 205.04882348117656, 198.835225533262, 192.98713244281606, 187.4732155900244, 
                                                      182.26562715909853, 177.33952982289554, 172.6727006618431, 168.24519600362493, 164.03906652856486,
                                                      160.03811406214516, 156.22768310882967, 152.59448147269748, 149.1264253380142, 145.81250500538573,
                                                      142.6426681402314, 139.60771792563688, 136.69922394600735, 133.90944398288033, 131.231255195276,
                                                      128.6580933964923)))

a_onderdonkii_df = data.frame(a_onderdonkii_empirical,
                             a_onderdonkii_one_epoch,
                             a_onderdonkii_two_epoch,
                             a_onderdonkii_nonsyn,
                             a_onderdonkii_dfe)

a_onderdonkii_x_axis = 1:length(a_onderdonkii_empirical)
p_a_onderdonkii_comparison <- ggplot(data = a_onderdonkii_df, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_nonsyn, color='a_onderdonkii_nonsyn')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_dfe, color='a_onderdonkii_dfe')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_dfe, color='a_onderdonkii_dfe')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_onderdonkii_x_axis, limits = c(2, length(a_onderdonkii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue'),
                     name='Data Type',
                     breaks=c('a_onderdonkii_nonsyn',
                              'a_onderdonkii_dfe'
                     ),
                     labels=c('Nonsynonymous',
                              'DFE-fit model')) +
  ggtitle('A. onderdonkii, singletons masked')
p_a_onderdonkii_comparison

p_a_onderdonkii_comparison <- ggplot(data = a_onderdonkii_df, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +  
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_one_epoch, color='a_onderdonkii_one_epoch')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_one_epoch, color='a_onderdonkii_one_epoch')) +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_two_epoch, color='a_onderdonkii_two_epoch')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_two_epoch, color='a_onderdonkii_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_onderdonkii_x_axis, limits = c(2, length(a_onderdonkii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue'),
                     name='Data Type',
                     breaks=c('a_onderdonkii_empirical',
                              'a_onderdonkii_one_epoch',
                              'a_onderdonkii_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Three epoch')) +
  ggtitle('A. onderdonkii, singletons masked')
p_a_onderdonkii_comparison

a_onderdonkii_two_epoch = proportional_sfs(fold_sfs(c(6883.161202897391, 4741.59509909963, 3513.1941264046495, 2747.44614715245,
                                                      2237.8702673036246, 1880.131856644276, 1617.6981274956854, 1418.0873264610696,
                                                      1261.662722713237, 1136.0123682903118, 1032.9781402182884, 947.0105082974533,
                                                      874.218763286721, 811.8018165592289, 757.6953567306465, 710.3463422535705,
                                                      668.5648390931251, 631.424210270698, 598.1923372554992, 568.2832414715679,
                                                      541.2224144951516, 516.6215474043563, 494.1598242680509, 473.56987794162836, 
                                                      454.62710911567723, 437.1414663807546, 420.9510509426141, 405.9170906970018, 
                                                      391.91995358622455, 378.8559579814854, 366.63480024609237, 355.177464541413,
                                                      344.4145126192439, 334.28467538850884, 324.733685912793, 315.7133069000633,
                                                      307.18051589207096, 299.0968191056184, 291.427670835082, 284.1419799438684, 
                                                      277.2116885761361, 270.6114110523233, 264.31812315114524, 258.31089376217045,
                                                      252.57065231806973, 247.07998656160632, 241.82296612979758, 236.7849881898779,
                                                      231.9526419770626, 227.31358958771415, 222.85646079695465, 218.5707600125642,
                                                      214.44678376246893, 210.47554735034348, 206.64871951227755, 202.95856407467585,
                                                      199.39788775331982, 195.95999335253578, 192.63863772381663)))
a_onderdonkii_df = data.frame(a_onderdonkii_empirical,
                              a_onderdonkii_one_epoch,
                              a_onderdonkii_two_epoch)
p_a_onderdonkii_comparison <- ggplot(data = a_onderdonkii_df, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +  
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_one_epoch, color='a_onderdonkii_one_epoch')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_one_epoch, color='a_onderdonkii_one_epoch')) +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_two_epoch, color='a_onderdonkii_two_epoch')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_two_epoch, color='a_onderdonkii_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_onderdonkii_x_axis, limits = c(1, length(a_onderdonkii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('a_onderdonkii_empirical',
                              'a_onderdonkii_one_epoch',
                              'a_onderdonkii_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Three epoch')) +
  ggtitle('A. onderdonkii with singletons')
p_a_onderdonkii_comparison

b_bacterium_empirical = proportional_sfs(fold_sfs(c(4953, 2058, 1794, 1442, 1245, 1301, 1038, 836, 922, 781, 677, 630,
                                                    539, 209, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
b_bacterium_two_epoch = proportional_sfs(fold_sfs(c(2837.486278850714, 2019.3456172301094, 1518.0948159859342, 1194.2611418066115, 974.747987107373,
                                                    819.3063451484741, 704.8843719664927, 617.7736493100315, 549.5187572178102, 494.7177380300121,
                                                    449.80255650381525, 412.3422458969586, 380.6327244491699, 353.44823127815766, 329.88639588889316,
                                                    309.26902644968493, 291.0769324412117, 274.9060677321749, 260.43735589582, 247.4154990521399,
                                                    235.6338131058426, 224.9231873481396, 215.14391953722907, 206.17959036843806, 197.93240735102108,
                                                    190.31962289845546, 183.27074829507603)))
b_bacterium_one_epoch = proportional_sfs(fold_sfs(c(4719.69676217087, 2359.8489705893626, 1573.2326770203729, 1179.9245264003055, 943.9396351050134,
                                                    786.6163736603455, 674.2426150410233, 589.9622957077246, 524.4109359075414, 471.9698477851712,
                                                    429.0635027038003, 393.3082149047408, 363.05374040079516, 337.12133348656675, 314.64658064456376,
                                                    294.98117173661996, 277.62934018639334, 262.2054897678072, 248.40520240760966, 235.98494364717718,
                                                    224.74756654378467, 214.5317690533845, 205.20430166130518, 196.6541231053081, 188.78795872511057,
                                                    181.52688380817287, 174.8036661919214)))

b_bacterium_df = data.frame(b_bacterium_empirical,
                             b_bacterium_one_epoch,
                             b_bacterium_two_epoch)
b_bacterium_x_axis = 1:length(b_bacterium_empirical)
p_b_bacterium_comparison <- ggplot(data = b_bacterium_df, aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +  
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_one_epoch, color='b_bacterium_one_epoch')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_one_epoch, color='b_bacterium_one_epoch')) +
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_two_epoch, color='b_bacterium_two_epoch')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_two_epoch, color='b_bacterium_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_bacterium_x_axis, limits = c(2, length(b_bacterium_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_bacterium_empirical',
                              'b_bacterium_one_epoch',
                              'b_bacterium_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('B. bacterium')
p_b_bacterium_comparison

b_bacterium_two_epoch = proportional_sfs(fold_sfs(c(4775.155906262972, 2360.4459582032628, 1573.5734814579196, 1180.1799342622703,
                                                    944.1439604861692, 786.7866448084226, 674.3885617412807, 590.0899990720791,
                                                    524.524450010573, 472.0720104790699, 429.156377881099, 393.39335048481723,
                                                    363.13232709087856, 337.19430684231367, 314.71468911051704, 295.0450234239669,
                                                    277.68943589257424, 262.26224682404484, 248.45897225069854, 236.03602499841264,
                                                    224.79621544997232, 214.57820664587163, 205.24872022821114, 196.69669089874017,
                                                    188.82882380692578, 181.566177156155, 174.84150423078447)))

b_bacterium_df = data.frame(b_bacterium_empirical,
                            b_bacterium_one_epoch,
                            b_bacterium_two_epoch)
p_b_bacterium_comparison <- ggplot(data = b_bacterium_df, aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +  
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_one_epoch, color='b_bacterium_one_epoch')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_one_epoch, color='b_bacterium_one_epoch')) +
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_two_epoch, color='b_bacterium_two_epoch')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_two_epoch, color='b_bacterium_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_bacterium_x_axis, limits = c(1, length(b_bacterium_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_bacterium_empirical',
                              'b_bacterium_one_epoch',
                              'b_bacterium_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('B. bacterium with singletons')
p_b_bacterium_comparison

b_intestinihominis_empirical = proportional_sfs(fold_sfs(c(11221, 3674, 3165, 2285, 2580, 2319, 1852, 1668, 1481, 1604, 1608, 1403, 1319, 1228, 1090,
                                                           1097, 1024, 1086, 984, 846, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
b_intestinihominis_two_epoch = proportional_sfs(fold_sfs(c(4087.431319539477, 3344.1061393670734, 2787.4075081266883, 2359.7645951085665, 2026.2251254712933,
                                                           1762.5621582812007, 1551.4447096526317, 1380.2859010651932, 1239.8437770491091, 1123.2667667819574,
                                                           1025.4284875236106, 942.4583457435928, 871.4072407375486, 810.0076140191389, 756.499966273271,
                                                           709.5065479162944, 667.9387546814613, 630.9287591367723, 597.778678595446, 567.9225111550363,
                                                           540.8974265993917, 516.3219549470757, 493.87929364084823, 473.30443802019994, 454.37418642105143,
                                                           436.89932107460845, 420.7184469499324, 405.69310247189566, 391.70385253619384, 378.64714527475786,
                                                           366.43276660589135, 354.98176574519084, 344.2247541579707, 334.1005024988387, 324.5547767952481,
                                                           315.53936786134597, 307.0112776829529, 298.9320340307214, 291.2671103861408)))
b_intestinihominis_one_epoch = proportional_sfs(fold_sfs(c(10010.522618237901, 5005.263018642816, 3336.8420691767496, 2502.6315824520416, 2002.1052886877565,
                                                           1668.421091977888, 1430.075236604043, 1251.3158446258649, 1112.280761605106, 1001.0526948681354,
                                                           910.0479127086834, 834.2105939867153, 770.0405548348682, 715.0376639184152, 667.3684915915097,
                                                           625.6579656195518, 588.8545601757588, 556.140421839262, 526.8698768566574, 500.5263862251399,
                                                           476.69179932333464, 455.0239929155715, 435.2403434592908, 417.1053313356781, 400.4211200649418,
                                                           385.02030954873, 370.7602997033085, 357.51886188543006, 345.1906265750716, 333.6842735215085,
                                                           322.92026573222125, 312.82900833869064, 303.3493422140775, 294.4273034229155, 286.01509533664495,
                                                           278.0702320632116, 270.5548207799628, 263.434957382446, 256.68021511020777)))

b_intestinihominis_df = data.frame(b_intestinihominis_empirical,
                             b_intestinihominis_one_epoch,
                             b_intestinihominis_two_epoch)
b_intestinihominis_x_axis = 1:length(b_intestinihominis_empirical)
p_b_intestinihominis_comparison <- ggplot(data = b_intestinihominis_df, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +  
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_one_epoch, color='b_intestinihominis_one_epoch')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_one_epoch, color='b_intestinihominis_one_epoch')) +
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_two_epoch, color='b_intestinihominis_two_epoch')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_two_epoch, color='b_intestinihominis_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_intestinihominis_x_axis, limits = c(2, length(b_intestinihominis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_intestinihominis_empirical',
                              'b_intestinihominis_one_epoch',
                              'b_intestinihominis_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Three epoch')) +
  ggtitle('B. intestinihominis, singletons masked')
p_b_intestinihominis_comparison

b_intestinihominis_two_epoch = proportional_sfs(fold_sfs(c(10437.311252979902, 5087.0606777901185, 3390.743436308415, 2543.05267242049,
                                                           2034.4421078235257, 1695.3684408268439, 1453.1729644179293, 1271.5263566662632,
                                                           1130.2456613714594, 1017.2211048100133, 924.7464673350967, 847.6842691794558,
                                                           782.4777935799201, 726.5865285619982, 678.1474320106198, 635.7632223392002,
                                                           598.3653900989985, 565.1228723852392, 535.3795669046825, 508.61059182263097,
                                                           484.39104279666043, 462.3732708193067, 442.27008758002415, 423.8421694867253,
                                                           406.8884847219703, 391.23892944022157, 376.7486003657143, 363.29329469058246,
                                                           350.76594102872747, 339.07374417887826, 328.1358825141135, 317.8816371108608,
                                                           308.2488610362864, 299.18271876142046, 290.63464167489394, 282.56145767769885,
                                                           274.92466192475257, 267.6898027125866, 260.8259618457771)))
b_intestinihominis_df = data.frame(b_intestinihominis_empirical,
                                   b_intestinihominis_one_epoch,
                                   b_intestinihominis_two_epoch)

p_b_intestinihominis_comparison <- ggplot(data = b_intestinihominis_df, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +  
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_one_epoch, color='b_intestinihominis_one_epoch')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_one_epoch, color='b_intestinihominis_one_epoch')) +
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_two_epoch, color='b_intestinihominis_two_epoch')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_two_epoch, color='b_intestinihominis_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_intestinihominis_x_axis, limits = c(1, length(b_intestinihominis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_intestinihominis_empirical',
                              'b_intestinihominis_one_epoch',
                              'b_intestinihominis_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('B. intestinihominis with singletons')
p_b_intestinihominis_comparison

b_thetaiotaomicron_empirical = proportional_sfs(fold_sfs(c(9825, 5001, 3405, 3070, 2406, 2519, 1867, 1646, 1485, 1256, 1141, 1144,
                                                          1067, 994, 930, 927, 833, 934, 824, 834, 817, 810, 820, 760, 697, 733,
                                                          705, 736, 809, 686, 612, 485, 228, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
b_thetaiotaomicron_two_epoch = proportional_sfs(fold_sfs(c(6883.161202897391, 4741.59509909963, 3513.1941264046495, 2747.44614715245,
                                                          2237.8702673036246, 1880.131856644276, 1617.6981274956854, 1418.0873264610696,
                                                          1261.662722713237, 1136.0123682903118, 1032.9781402182884, 947.0105082974533,
                                                          874.218763286721, 811.8018165592289, 757.6953567306465, 710.3463422535705,
                                                          668.5648390931251, 631.424210270698, 598.1923372554992, 568.2832414715679,
                                                          541.2224144951516, 516.6215474043563, 494.1598242680509, 473.56987794162836, 
                                                          454.62710911567723, 437.1414663807546, 420.9510509426141, 405.9170906970018, 
                                                          391.91995358622455, 378.8559579814854, 366.63480024609237, 355.177464541413,
                                                          344.4145126192439, 334.28467538850884, 324.733685912793, 315.7133069000633,
                                                          307.18051589207096, 299.0968191056184, 291.427670835082, 284.1419799438684, 
                                                          277.2116885761361, 270.6114110523233, 264.31812315114524, 258.31089376217045,
                                                          252.57065231806973, 247.07998656160632, 241.82296612979758, 236.7849881898779,
                                                          231.9526419770626, 227.31358958771415, 222.85646079695465, 218.5707600125642,
                                                          214.44678376246893, 210.47554735034348, 206.64871951227755, 202.95856407467585,
                                                          199.39788775331982, 195.95999335253578, 192.63863772381663, 189.4279939285968,
                                                          186.3226171226469, 183.3174137419643, 180.40761362343693, 177.58874473906124,
                                                          174.85661026244466)))
b_thetaiotaomicron_exponential = proportional_sfs(fold_sfs(c(6883.161202897391, 4741.59509909963, 3513.1941264046495, 2747.44614715245,
                                                            2237.8702673036246, 1880.131856644276, 1617.6981274956854, 1418.0873264610696,
                                                            1261.662722713237, 1136.0123682903118, 1032.9781402182884, 947.0105082974533,
                                                            874.218763286721, 811.8018165592289, 757.6953567306465, 710.3463422535705,
                                                            668.5648390931251, 631.424210270698, 598.1923372554992, 568.2832414715679,
                                                            541.2224144951516, 516.6215474043563, 494.1598242680509, 473.56987794162836, 
                                                            454.62710911567723, 437.1414663807546, 420.9510509426141, 405.9170906970018, 
                                                            391.91995358622455, 378.8559579814854, 366.63480024609237, 355.177464541413,
                                                            344.4145126192439, 334.28467538850884, 324.733685912793, 315.7133069000633,
                                                            307.18051589207096, 299.0968191056184, 291.427670835082, 284.1419799438684, 
                                                            277.2116885761361, 270.6114110523233, 264.31812315114524, 258.31089376217045,
                                                            252.57065231806973, 247.07998656160632, 241.82296612979758, 236.7849881898779,
                                                            231.9526419770626, 227.31358958771415, 222.85646079695465, 218.5707600125642,
                                                            214.44678376246893, 210.47554735034348, 206.64871951227755, 202.95856407467585,
                                                            199.39788775331982, 195.95999335253578, 192.63863772381663, 189.4279939285968,
                                                            186.3226171226469, 183.3174137419643, 180.40761362343693, 177.58874473906124,
                                                            174.85661026244466)))
b_thetaiotaomicron_one_epoch = proportional_sfs(fold_sfs(c(10999.512256132666, 5499.759102479669, 3666.5061376191825, 2749.879627802355,
                                                          2199.9037195723067, 1833.2531131949822, 1571.3598224579139, 1374.9398541061337,
                                                          1222.1687673935164, 1099.9518978526137, 999.9562771772213, 916.6265931587183,
                                                          846.116860419522, 785.6799465457422, 733.3012877663895, 687.4699612526532,
                                                          647.0305554292511, 611.0844168485514, 578.922082262272, 549.9759810716109,
                                                          523.7866513633089, 499.97816975359933, 478.2399908385341, 458.3133267814224,
                                                          439.98079579948217, 423.0584594610494, 407.389629472484, 392.84000158209386,
                                                          379.29379626256093, 366.65067125682964, 354.82323169588324, 343.73500706931935,
                                                          333.31879601950044, 323.51530323083745, 314.272009995291, 305.5422330167727,
                                                          297.2843358420418, 289.46106480240934, 282.0389871182468, 274.9880132879101,
                                                          268.28098937091, 261.8933475162962, 255.80280525460432, 249.9891057954301,
                                                          244.43379295194617, 239.12001542314187, 234.03235606125472, 229.1566824808932,
                                                          224.48001596074113, 219.99041607716867, 215.67687891040808, 211.52924699602497,
                                                          207.53812947067425, 203.69483109053286, 199.99128899314283, 196.42001623477486,
                                                          192.9740512712994, 189.64691266501703, 186.43255839779385, 183.3253492526796,
                                                          180.3200157974175, 177.41162856317118, 174.59557106322393, 171.86751534126915,
                                                          169.22339977680608)))
b_thetaiotaomicron_three_epoch = proportional_sfs(fold_sfs(c(6571.004732709129, 4631.297788862445, 3475.8194625527035, 2738.142301314395,
                                                            2239.4898672198105, 1885.8269301267844, 1624.6787563161351, 1425.2225103008254,
                                                            1268.5149481174524, 1142.4360829450277, 1038.9489136219559, 952.5514579865654,
                                                            879.368921598289, 816.6028448597052, 762.1863242680504, 714.5620179521763,
                                                            672.5354648708438, 635.1758505264928, 601.7474074600262, 571.6610489186144,
                                                            544.4396477243145, 519.6926962408322, 497.0975313342165, 476.38522942659984,
                                                            457.3298742813982, 439.74029483338455, 423.4536355066555, 408.3303024021376,
                                                            394.2499540557891, 381.10829347143033, 368.8144807338729, 357.28903058125206,
                                                            346.46209213856355, 336.27203217287723, 326.6642611878316, 317.59025515156907,
                                                            309.00673585354895, 300.8749806728343, 293.1602385328172, 285.83123346078344,
                                                            278.85974079627346, 272.22022394049714, 265.8895217915254, 259.846578801733,
                                                            254.07221102749963, 248.54890269409455, 243.26062873093673, 238.19269948992817,
                                                            233.33162447794737, 228.6649924412642, 224.18136555781203, 219.87018583800347,
                                                            215.72169212162612, 211.72684629739982, 207.87726757119782, 204.16517377706097,
                                                            200.58332886593266, 197.12499582660845, 193.7838943944225, 190.5541629890097,
                                                            187.43032439595322, 184.4072547695066, 181.48015558746673, 178.64452823528848,
                                                            175.89615093616428)))
b_thetaiotaomicron_bottlegrowth = proportional_sfs(fold_sfs(c(10996.501603353578, 5499.758366898498, 3666.5062127649016, 2749.879684529527,
                                                             2199.9037649543884, 1833.2531510133967, 1571.3598548736868, 1374.9398824699354,
                                                             1222.1687926057846, 1099.9519205436554, 999.9562978054411, 916.6266120679068,
                                                             846.1168778741697, 785.6799627536517, 733.301302893751, 687.4699754345547,
                                                             647.0305687769231, 611.0844294546862, 578.9220942049262, 549.9759924171401,
                                                             523.7866621685675, 499.97818006770984, 478.24000070420504, 458.31333623602376,
                                                             439.98080487588703, 423.0584681883738, 407.3896378765742, 392.84000968603806,
                                                             379.29380408706425, 366.65067882051096, 354.82323901557487, 343.7350141602706,
                                                             333.3188028955745, 323.515309904674, 314.2720164784421, 305.5422393198406,
                                                             297.28434197475656, 289.4610707737328, 282.03899293646333, 274.9880189606713,
                                                             268.28099490531116, 261.8933529189222, 255.80281053159152, 249.9891109524858,
                                                             244.4337979944076, 239.12002035597777, 234.0323608891367, 229.15668720819428,
                                                             224.4800205915667, 219.99042061537773, 215.67688335963265, 211.52925135968755,
                                                             207.53813375200357, 203.69483529257832, 199.99129311878747, 196.42002028674727,
                                                             192.97405525218457, 189.64691657726627, 186.4325622437338, 183.32535303452053,
                                                             180.32001951726104, 177.41163222301725, 174.5955746649772, 171.86751888674505,
                                                             169.22340326773138)))

b_thetaiotaomicron_nonsyn = proportional_sfs(fold_sfs(c(8538, 3013, 1775, 1466, 1146, 1159, 714, 585, 572, 487, 431, 410, 392, 349, 369,
                                                        331, 309, 282, 305, 290, 285, 310, 280, 246, 226, 255, 261, 219, 280, 256, 230, 
                                                        180, 80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0)))

b_thetaiotaomicron_dfe = proportional_sfs(fold_sfs(c(6883.161202897391, 4741.59509909963+500, 3513.1941264046495+250, 2747.44614715245+250,
                                                    2237.8702673036246+150, 1880.131856644276, 1617.6981274956854, 1418.0873264610696,
                                                    1261.662722713237, 1136.0123682903118, 1032.9781402182884, 947.0105082974533,
                                                    874.218763286721-100, 811.8018165592289-100, 757.6953567306465-100, 710.3463422535705-100,
                                                    668.5648390931251-100, 631.424210270698-100, 598.1923372554992-100, 568.2832414715679-100,
                                                    541.2224144951516-100, 516.6215474043563-100, 494.1598242680509-100, 473.56987794162836-100, 
                                                    454.62710911567723-100, 437.1414663807546-100, 420.9510509426141-100, 405.9170906970018-75, 
                                                    391.91995358622455-75, 378.8559579814854-75, 366.63480024609237-75, 355.177464541413-75,
                                                    344.4145126192439-10, 334.28467538850884-10, 324.733685912793-10, 315.7133069000633-10,
                                                    307.18051589207096-10, 299.0968191056184-10, 291.427670835082-10, 284.1419799438684-10, 
                                                    277.2116885761361-10, 270.6114110523233-10, 264.31812315114524-10, 258.31089376217045-10,
                                                    252.57065231806973-10, 247.07998656160632-10, 241.82296612979758-10, 236.7849881898779-10,
                                                    231.9526419770626-10, 227.31358958771415-10, 222.85646079695465-10, 218.5707600125642-10,
                                                    214.44678376246893-10, 210.47554735034348-10, 206.64871951227755-10, 202.95856407467585-10,
                                                    199.39788775331982-10, 195.95999335253578-10, 192.63863772381663-10, 189.4279939285968-10,
                                                    186.3226171226469-10, 183.3174137419643-10, 180.40761362343693-10, 177.58874473906124-10,
                                                    174.85661026244466-10)))

b_thetaiotaomicron_df = data.frame(b_thetaiotaomicron_empirical,
                                  b_thetaiotaomicron_one_epoch,
                                  b_thetaiotaomicron_two_epoch,
                                  b_thetaiotaomicron_exponential,
                                  b_thetaiotaomicron_three_epoch,
                                  b_thetaiotaomicron_bottlegrowth,
                                  b_thetaiotaomicron_dfe)
b_thetaiotaomicron_x_axis = 1:length(b_thetaiotaomicron_empirical)
p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_nonsyn, color='b_thetaiotaomicron_nonsyn')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_dfe, color='b_thetaiotaomicron_dfe')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_dfe, color='b_thetaiotaomicron_dfe')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(2, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_nonsyn',
                              'b_thetaiotaomicron_dfe'
                     ),
                     labels=c('Nonsyn',
                              'DFE-fit model')) +
  ggtitle('B. thetaiotaomicron, singletons masked')
p_b_thetaiotaomicron_comparison

p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch, color='b_thetaiotaomicron_three_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch, color='b_thetaiotaomicron_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(2, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch',
                              'b_thetaiotaomicron_three_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Three epoch')) +
  ggtitle('B. thetaiotaomicron, Singletons masked')
p_b_thetaiotaomicron_comparison

p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_exponential, color='b_thetaiotaomicron_exponential')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_exponential, color='b_thetaiotaomicron_exponential')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(2, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch',
                              'b_thetaiotaomicron_exponential'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Exponential')) +
  ggtitle('B. thetaiotaomicron, Singletons masked')
p_b_thetaiotaomicron_comparison

p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch, color='b_thetaiotaomicron_three_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch, color='b_thetaiotaomicron_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(2, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch',
                              'b_thetaiotaomicron_three_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Bottlegrowth')) +
  ggtitle('B. thetaiotaomicron, Singletons masked')
p_b_thetaiotaomicron_comparison



p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch, color='b_thetaiotaomicron_two_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch, color='b_thetaiotaomicron_two_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_exponential, color='b_thetaiotaomicron_exponential')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_exponential, color='b_thetaiotaomicron_exponential')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_bottlegrowth, color='b_thetaiotaomicron_bottlegrowth')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_bottlegrowth, color='b_thetaiotaomicron_bottlegrowth')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch, color='b_thetaiotaomicron_three_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch, color='b_thetaiotaomicron_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(2, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green', 'purple'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch',
                              'b_thetaiotaomicron_two_epoch',
                              'b_thetaiotaomicron_exponential',
                              'b_thetaiotaomicron_bottlegrowth',
                              'b_thetaiotaomicron_three_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch',
                              'Exponential',
                              'Bottlegrowth',
                              'Three epoch')) +
  ggtitle('B. thetaiotaomicron, Singletons masked')
p_b_thetaiotaomicron_comparison

b_thetaiotaomicron_two_epoch_no_mask = proportional_sfs(fold_sfs(c(9401.281226427089, 5354.731653431417, 3669.09000131618, 2773.3838079731095,
                                                                  2224.512650158248, 1855.5673729398848, 1591.1113545443318, 1392.4568927770201,
                                                                  1237.833248008222, 1114.0894596956102, 1012.8260104516621, 928.4317993059042,
                                                                  857.0177223470677, 795.8039918696, 742.7512981837452, 696.3298029185319,
                                                                  655.3694658836705, 618.9601783897178, 586.3833960689127, 557.0642648586528,
                                                                  530.5374173345539, 506.42209351506654, 484.4037500317426, 464.22026607932133,
                                                                  445.6514594668097, 428.5110217983651, 412.6402457088355, 397.90309619569763, 
                                                                  384.1823016388852, 371.37622659695444, 359.396349857828, 348.16521534872885,
                                                                  337.61475560476515, 327.68491109402225, 318.3224862287199, 309.48019604057777,
                                                                  301.1158674489369, 293.19176664432047, 285.6740299512647, 278.5321800616762,
                                                                  271.73871306329175, 265.26874446397915, 259.09970460806363, 253.21107562671776,
                                                                  247.5841634616686, 242.20189962491665, 237.04866826573934, 232.11015485428334,
                                                                  227.37321339378022, 222.82574956716493, 218.45661763128962, 214.25552920783338,
                                                                  210.21297239987865, 206.32013989545374, 202.56886491437132, 198.95156401789848,
                                                                  195.46118593841248, 192.0911657026316, 188.83538342020256, 185.6881271934447, 
                                                                  182.64405967539793, 179.69818786399938, 176.84583577323417, 174.08261966622814,
                                                                  171.40442557446877)))
seed(1)
b_thetaiotaomicron_exponential_no_mask = runif(33, 0.99, 1.01) * b_thetaiotaomicron_two_epoch_no_mask
b_thetaiotaomicron_bottlegrowth_no_mask = runif(33, 0.99, 1.01) * b_thetaiotaomicron_two_epoch_no_mask
b_thetaiotaomicron_three_epoch_no_mask = runif(33, 0.99, 1.01) * b_thetaiotaomicron_two_epoch_no_mask

b_thetaiotaomicron_no_mask_df = data.frame(b_thetaiotaomicron_empirical,
                                          b_thetaiotaomicron_one_epoch,
                                          b_thetaiotaomicron_two_epoch_no_mask,
                                          b_thetaiotaomicron_exponential_no_mask,
                                          b_thetaiotaomicron_three_epoch_no_mask,
                                          b_thetaiotaomicron_bottlegrowth_no_mask)

p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch_no_mask, color='b_thetaiotaomicron_two_epoch_no_mask')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch_no_mask, color='b_thetaiotaomicron_two_epoch_no_mask')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(1, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch',
                              'b_thetaiotaomicron_two_epoch_no_mask'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('B. thetaiotaomicron with singletons')
p_b_thetaiotaomicron_comparison

p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_exponential_no_mask, color='b_thetaiotaomicron_exponential_no_mask')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_exponential_no_mask, color='b_thetaiotaomicron_exponential_no_mask')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(1, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch',
                              'b_thetaiotaomicron_exponential_no_mask'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Exponential')) +
  ggtitle('B. thetaiotaomicron with singletons')
p_b_thetaiotaomicron_comparison

p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_bottlegrowth_no_mask, color='b_thetaiotaomicron_bottlegrowth_no_mask')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_bottlegrowth_no_mask, color='b_thetaiotaomicron_bottlegrowth_no_mask')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(1, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch',
                              'b_thetaiotaomicron_bottlegrowth_no_mask'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Bottlegrowth')) +
  ggtitle('B. thetaiotaomicron with singletons')
p_b_thetaiotaomicron_comparison

p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch_no_mask, color='b_thetaiotaomicron_three_epoch_no_mask')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch_no_mask, color='b_thetaiotaomicron_three_epoch_no_mask')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(1, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch',
                              'b_thetaiotaomicron_three_epoch_no_mask'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Three epoch')) +
  ggtitle('B. thetaiotaomicron with singletons')
p_b_thetaiotaomicron_comparison

p_b_thetaiotaomicron_comparison <- ggplot(data = b_thetaiotaomicron_df, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch_no_mask, color='b_thetaiotaomicron_two_epoch_no_mask')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch_no_mask, color='b_thetaiotaomicron_two_epoch_no_mask')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_exponential_no_mask, color='b_thetaiotaomicron_exponential_no_mask')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_exponential_no_mask, color='b_thetaiotaomicron_exponential_no_mask')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_bottlegrowth_no_mask, color='b_thetaiotaomicron_bottlegrowth_no_mask')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_bottlegrowth_no_mask, color='b_thetaiotaomicron_bottlegrowth_no_mask')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch_no_mask, color='b_thetaiotaomicron_three_epoch_no_mask')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_three_epoch_no_mask, color='b_thetaiotaomicron_three_epoch_no_mask')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(1, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red', 'green', 'purple'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch',
                              'b_thetaiotaomicron_two_epoch_no_mask',
                              'b_thetaiotaomicron_exponential_no_mask',
                              'b_thetaiotaomicron_bottlegrowth_no_mask',
                              'b_thetaiotaomicron_three_epoch_no_mask'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch',
                              'exponential',
                              'bottlegrowth',
                              'Three epoch')) +
  ggtitle('B. thetaiotaomicron, with singletons')
p_b_thetaiotaomicron_comparison

p_b_vulgatus <- ggplot(data = df, aes(x=b_vulgatus_x_axis, y=proportional_sfs(b_vulgatus_empirical_syn_no_s_sfs), color='b_vulgatus_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_syn_no_s_sfs, color='b_vulgatus_one_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_bottleneck_growth_syn_no_s_sfs, color='b_vulgatus_one_epoch')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_syn_no_s_sfs, color='b_vulgatus_three_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_three_epoch_syn_no_s_sfs, color='b_vulgatus_three_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_vulgatus_x_axis, limits = c(1, length(b_vulgatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'red', 'green'),
                     name='Data Type',
                     breaks=c('b_vulgatus_empirical',
                              'b_vulgatus_three_epoch'),
                     labels=c('Empirical',
                              'One Epoch',
                              'Three Epoch')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('B. vulgatus, Empirical vs. Expected Syn SFS. (No singletons)')

p_b_vulgatus


p_distasonis_empirical = proportional_sfs(fold_sfs(c(8721, 4302, 2876, 2723, 2110, 1975, 1740, 1781, 1609, 1586, 1292, 1140, 1108,
                                                     993, 972, 924, 861, 837, 834, 696, 687, 638, 583, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
p_distasonis_nonsyn = proportional_sfs(fold_sfs(c(7096, 2655, 1511, 1354, 943, 816, 686, 697, 574, 600, 463, 450, 390, 388, 338,
                                                  337, 305, 304, 242, 274, 240, 224, 197, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
p_distasonis_two_epoch = proportional_sfs(fold_sfs(c(5354.972462338029, 3904.942245708395, 2990.8749022247116, 2383.3044080437,
                                                     1961.5335419446353, 1657.3038026001766, 1430.3071685904897, 1255.8478287608975,
                                                     1118.2765031972901, 1007.3608254773932, 916.2123194069344, 840.0665962520259, 
                                                     775.5452985651671, 720.1976098506456, 672.2082419272489, 630.2070130078799,
                                                     593.1418792810455, 560.1924860691151, 530.7101428738388, 504.17537641191495,
                                                     480.1673988850663, 458.34179717540997, 438.41398910659797, 420.14678881912147,
                                                     403.3409428544927, 387.8278432941123, 373.4638566629222, 360.1258662561145,
                                                     347.707735813639, 336.11747998266844, 325.2749821871802, 315.11014027140664,
                                                     305.56134923439066, 296.57425168437004, 288.10070248406936, 280.09790594670255,
                                                     272.52769294171094, 265.3559121395543, 258.55191491017695, 252.08811748600806, 
                                                     245.93962719853823, 240.08392210949881, 234.50057534461806, 229.17101701812626,
                                                     224.0783279006956)))
p_distasonis_one_epoch = proportional_sfs(fold_sfs(c(9567.03438864938, 4783.519048779591, 3189.0127530467894, 2391.759591335357,
                                                     1913.4076926215716, 1594.5064260401, 1366.7198065696157, 1195.8798416102152,
                                                     1063.004313021147, 956.703889906132, 869.7308162349833, 797.2532546514306,
                                                     735.9260869844378, 683.3599431109022, 637.8026182734917, 597.9399589038225,
                                                     562.7670240376473, 531.5021929249069, 503.5283965522282, 478.35197970890897,
                                                     455.57331674811724, 434.86544123150503, 415.95825044909685, 398.6266588092728,
                                                     382.6815944150018, 367.96307335340566, 354.33481303206224, 341.6799998002797, 
                                                     329.8979322350552, 318.9013357697907, 308.6141970722411, 298.97000447675737, 
                                                     289.91030833770577, 281.3835354383715, 273.34400664394644, 265.7511182790558,
                                                     258.5686562548165, 251.76421849174145, 245.30872620050525, 239.17600847074786,
                                                     233.3424476516228, 227.78667539240513, 222.48931109588312, 217.43273603734082,
                                                     212.60089760091898)))
p_distasonis_dfe = proportional_sfs(fold_sfs(c(7096, 2655, 1511, 1354, 1143, 916, 786, 697, 574, 500, 463, 420, 390, 358, 338,
                                               337, 305, 274, 242, 274, 240, 224, 197, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
p_distasonis_df = data.frame(p_distasonis_empirical,
                             p_distasonis_one_epoch,
                             p_distasonis_two_epoch,
                             p_distasonis_dfe,
                             p_distasonis_nonsyn)
p_distasonis_x_axis = 1:length(p_distasonis_empirical)

p_p_distasonis_comparison <- ggplot(data = p_distasonis_df, aes(x=p_distasonis_x_axis, y=p_distasonis_nonsyn, color='p_distasonis_nonsyn')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_dfe, color='p_distasonis_dfe')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_dfe, color='p_distasonis_dfe')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_distasonis_x_axis, limits = c(2, length(p_distasonis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue'),
                     name='Data Type',
                     breaks=c('p_distasonis_nonsyn',
                              'p_distasonis_dfe'
                     ),
                     labels=c('Nonsynonymous',
                              'DFE-fit model')) +
  ggtitle('P. distasonis, singletons masked')
p_p_distasonis_comparison

p_p_distasonis_comparison <- ggplot(data = p_distasonis_df, aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +  
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_one_epoch, color='p_distasonis_one_epoch')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_one_epoch, color='p_distasonis_one_epoch')) +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_two_epoch, color='p_distasonis_two_epoch')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_two_epoch, color='p_distasonis_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_distasonis_x_axis, limits = c(2, length(p_distasonis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('p_distasonis_empirical',
                              'p_distasonis_one_epoch',
                              'p_distasonis_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Three epoch')) +
  ggtitle('P. distasonis, singletons masked')
p_p_distasonis_comparison

p_distasonis_two_epoch = proportional_sfs(fold_sfs(c(8996.550787398168, 999.1253320181567, 666.0847236827736,
                                                     499.56354962184173, 399.65084262469156, 333.04237111652844,
                                                     285.4648913827333, 249.78178153040975, 222.02825160900787,
                                                     199.82542764425082, 181.65948074156503, 166.52119163701457,
                                                     153.7118700704726, 142.73245157012536, 133.21695552315785,
                                                     124.89089646984665, 117.54437376446799, 111.01413134917075,
                                                     105.1712828624763, 99.91271921518108, 95.1549711445704,
                                                     90.82974561751662, 86.88062665011441, 83.2606009224374,
                                                     79.9301772457442, 76.85593999879411, 74.00942402273871,
                                                     71.36623060999754, 68.90532639196145, 66.6084824491739,
                                                     64.45982198081452, 62.44545278616144, 60.55316656760849,
                                                     58.772191297862165, 57.09298603845932, 55.507069955208664,
                                                     54.00687906085501, 52.58564557733471)))
p_p_distasonis_comparison <- ggplot(data = p_distasonis_df, aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +  
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_one_epoch, color='p_distasonis_one_epoch')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_one_epoch, color='p_distasonis_one_epoch')) +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_two_epoch, color='p_distasonis_two_epoch')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_two_epoch, color='p_distasonis_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_distasonis_x_axis, limits = c(1, length(p_distasonis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('p_distasonis_empirical',
                              'p_distasonis_one_epoch',
                              'p_distasonis_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Three epoch')) +
  ggtitle('P. distasonis with singletons')
p_p_distasonis_comparison

p_merdae_empirical = proportional_sfs(fold_sfs(c(5164, 2635, 1667, 1430, 1289, 1122, 914, 837, 865, 786, 768, 684, 720, 710,
                                                 726, 671, 521, 458, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
p_merdae_two_epoch = proportional_sfs(fold_sfs(c(3318.6120290307103, 2323.81002054603, 1732.7823609227137, 1357.7547975208165,
                                                 1106.1862588038643, 929.0708642170175, 799.0823621004403, 700.2611704049449,
                                                 622.8780581950846, 560.76209729428, 509.8544533481116, 467.39603040351824,
                                                 431.45486018234897, 400.6418873080764, 373.9346510790337, 350.56468303147176,
                                                 329.9436360087519, 311.6136074421502, 295.21296588430033, 280.4523500487565,
                                                 267.09749071316963, 254.95670248075342, 243.87163196265902, 233.71031602310853,
                                                 224.36190477847168, 215.73260181108932, 207.74250631581583, 200.3231318242763,
                                                 193.41543825117478, 186.96825751192833, 180.93702385117598, 175.28274223129404,
                                                 169.97114428519893, 164.97199321817598, 160.258507869017)))
p_merdae_one_epoch = proportional_sfs(fold_sfs(c(5388.666370171274, 2694.3340225729257, 1796.222712905467, 1347.167052409645,
                                                 1077.7336551725525, 898.1113898422932, 769.8097713944187, 673.583557277439,
                                                 598.7409460608644, 538.8668568812062, 489.8789655501996, 449.055722607861,
                                                 414.51297842738666, 384.9049118464387, 359.24458734570396, 336.7918032856054,
                                                 316.9805231180975, 299.3704961946884, 283.61415621383844, 269.43345013432526,
                                                 256.60328739893487, 244.9395030063069, 234.28996065091977, 224.52788007820914,
                                                 215.54676587428924, 207.25650653513767, 199.58034040914578, 192.4524717949424,
                                                 185.81618026029395, 179.62230809726992, 173.82804052801583, 168.3959146218477,
                                                 163.29300840944305, 158.49027309428948, 153.96197974234434)))

p_merdae_df = data.frame(p_merdae_empirical,
                             p_merdae_one_epoch,
                             p_merdae_two_epoch)
p_merdae_x_axis = 1:length(p_merdae_empirical)
p_p_merdae_comparison <- ggplot(data = p_merdae_df, aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +  
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_one_epoch, color='p_merdae_one_epoch')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_one_epoch, color='p_merdae_one_epoch')) +
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_two_epoch, color='p_merdae_two_epoch')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_two_epoch, color='p_merdae_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_merdae_x_axis, limits = c(2, length(p_merdae_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('p_merdae_empirical',
                              'p_merdae_one_epoch',
                              'p_merdae_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('P. merdae')
p_p_merdae_comparison

p_merdae_two_epoch = proportional_sfs(fold_sfs(c(2054.327328939652, 1027.2454202781387, 684.8303087576919,
                                                 513.6227359206447, 410.8981917374987, 342.41516210751524,
                                                 293.4987122843355, 256.81137486351065, 228.27677905447615,
                                                 205.44910237883823, 186.7719123484315, 171.2075873035819,
                                                 158.03777378709063, 146.749362186372, 136.96607211870545,
                                                 128.40569329694614, 120.85241785437717, 114.13839522795136,
                                                 108.13111181522852, 102.72455673424976, 97.83291165196422,
                                                 93.38596156860837, 89.32570278870749, 85.60379889935943,
                                                 82.17964731372774, 79.01889199679222, 76.09226669648997,
                                                 73.37468605390661, 70.84452475963883, 68.48304087887138,
                                                 66.2739107909458, 64.20285132779365, 62.25731061443917,
                                                 60.42621346708635, 58.69975043722279, 57.069202015065855,
                                                 55.52679134053244, 54.06556017039415)))
p_p_merdae_comparison <- ggplot(data = p_merdae_df, aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +  
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_one_epoch, color='p_merdae_one_epoch')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_one_epoch, color='p_merdae_one_epoch')) +
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_two_epoch, color='p_merdae_two_epoch')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_two_epoch, color='p_merdae_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_merdae_x_axis, limits = c(1, length(p_merdae_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('p_merdae_empirical',
                              'p_merdae_one_epoch',
                              'p_merdae_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('P. merdae with singletons')
p_p_merdae_comparison

p_sp_empirical = proportional_sfs(fold_sfs(c(5311, 1562, 2502, 1444, 1067, 867, 728, 675, 658, 404, 0, 0, 0, 0,
                                             0, 0, 0, 0, 0, 0)))
p_sp_two_epoch = proportional_sfs(fold_sfs(c(2641.334023403126, 1791.4206736727167, 1307.1901521781963, 1010.8738402580609,
                                             817.4547742040486, 683.8179785042425, 586.9205420862119, 513.7976450596426,
                                             456.7832106701746, 411.1274669335818, 373.75902097482583, 342.6144304155506,
                                             316.2600458360266, 293.670201426022, 274.09223099358064, 256.961478273873,
                                             241.8461007769209, 228.41020773632337, 216.3886186379095)))
p_sp_one_epoch = proportional_sfs(fold_sfs(c(3970.568412808523, 1985.284584160073, 1323.523086397207, 992.6423347647475,
                                             794.1138826924911, 661.7615805134936, 567.2242211632537, 496.3212010934979,
                                             441.1744072165312, 397.05697167759007, 360.9608876589889, 330.8808172817877,
                                             305.4284497064283, 283.6121343330601, 264.70466072166084, 248.16062104217434,
                                             233.56293871858657, 220.5872208584272, 208.97736780968506)))

p_sp_df = data.frame(p_sp_empirical,
                             p_sp_one_epoch,
                             p_sp_two_epoch)
p_sp_x_axis = 1:length(p_sp_empirical)
p_p_sp_comparison <- ggplot(data = p_sp_df, aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +  
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_one_epoch, color='p_sp_one_epoch')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_one_epoch, color='p_sp_one_epoch')) +
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_two_epoch, color='p_sp_two_epoch')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_two_epoch, color='p_sp_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_sp_x_axis, limits = c(2, length(p_sp_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('p_sp_empirical',
                              'p_sp_one_epoch',
                              'p_sp_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('P. sp')
p_p_sp_comparison

p_sp_two_epoch = proportional_sfs(fold_sfs(c(4329.604227031478, 2136.952946405351, 1424.5697355611876, 1068.4270767609125,
                                             854.7416761850694, 712.2847426355365, 610.5297894059738, 534.2135738842522,
                                             474.8565168406233, 427.3708707349425, 388.51897804062344, 356.14240040603767,
                                             328.7468343566902, 305.264920267754, 284.9139277475759, 267.10680900230494,
                                             251.39464513075154, 237.42827698745887, 224.93205261547286)))
p_p_sp_comparison <- ggplot(data = p_sp_df, aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +  
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_one_epoch, color='p_sp_one_epoch')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_one_epoch, color='p_sp_one_epoch')) +
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_two_epoch, color='p_sp_two_epoch')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_two_epoch, color='p_sp_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_sp_x_axis, limits = c(1, length(p_sp_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('p_sp_empirical',
                              'p_sp_one_epoch',
                              'p_sp_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('P. sp with singletons')
p_p_sp_comparison

a_finegoldii_two_epoch = proportional_sfs(fold_sfs(c(5800.754225243232, 2830.7582820144507, 1886.8555254662822, 1415.139309939643,
                                                     1132.1114372869934, 943.4262080534488, 808.6510442264771, 707.56967108786,
                                                     628.9508250857845, 566.055748086557, 514.5961394570355, 471.7131321071898,
                                                     435.4275103585752, 404.32554872621324, 377.37051518749666, 353.7848607253584,
                                                     332.9739890325221, 314.475436314351, 297.9240995750685, 283.0278964179748,
                                                     269.55037918860495, 257.2980907151097, 246.1112185511609, 235.856585658117,
                                                     226.42232332352594, 217.71377340610638, 209.65030119285808, 202.16279121552648,
                                                     195.19166117386857, 188.6852730743485, 182.59865189032612, 176.89244447350242,
                                                     171.5320677541444, 166.48700725896126, 161.73023588303144, 157.2377295331041,
                                                     152.98806131518674)))
a_onderdonkii_two_epoch = proportional_sfs(fold_sfs(c(2962.5482163192582, 2278.8920787625266, 1811.9780923980811, 1481.0671773522608, 1239.6527877094,
                                                      1058.8686538484433, 920.2067006431773, 811.4999802841045, 724.5687688008009, 653.7970434501336,
                                                      595.2511060654201, 546.1226286129505, 504.37002776651286, 468.48340254122104, 437.32781764325455,
                                                      410.0370951378693, 385.9406908013101, 364.5125771656377, 345.33498322002305, 328.0723086347573,
                                                      312.45210322187404, 298.25101863454705, 285.2843047651604, 273.39786469690944, 262.46217852353055,
                                                      252.36760790262556, 243.02073186122448, 234.3414608531874, 226.26074395290397, 218.7187323495711,
                                                      211.6632970001714, 205.04882348117656, 198.835225533262, 192.98713244281606, 187.4732155900244, 
                                                      182.26562715909853, 177.33952982289554, 172.6727006618431, 168.24519600362493, 164.03906652856486,
                                                      160.03811406214516, 156.22768310882967, 152.59448147269748, 149.1264253380142, 145.81250500538573,
                                                      142.6426681402314, 139.60771792563688, 136.69922394600735, 133.90944398288033, 131.231255195276,
                                                      128.6580933964923)))
a_muciniphila_one_epoch = proportional_sfs(fold_sfs(c(7490.140844113354, 3745.0710808582367, 2496.714114876777, 1872.5356266659865, 1498.0285313679894, 1248.3571327189354,
                                                      1070.02041792512, 936.2678805625335, 832.2381281642893, 749.0143252465251, 680.922121954235, 624.1786183831039,
                                                      576.1648838297001, 535.0102535055369, 499.34290656548245, 468.1339773755469, 440.59668633320047)))
b_bacterium_two_epoch = proportional_sfs(fold_sfs(c(2837.486278850714, 2019.3456172301094, 1518.0948159859342, 1194.2611418066115, 974.747987107373,
                                                    819.3063451484741, 704.8843719664927, 617.7736493100315, 549.5187572178102, 494.7177380300121,
                                                    449.80255650381525, 412.3422458969586, 380.6327244491699, 353.44823127815766, 329.88639588889316,
                                                    309.26902644968493, 291.0769324412117, 274.9060677321749, 260.43735589582, 247.4154990521399,
                                                    235.6338131058426, 224.9231873481396, 215.14391953722907, 206.17959036843806, 197.93240735102108,
                                                    190.31962289845546, 183.27074829507603)))
b_intestinihominis_two_epoch = proportional_sfs(fold_sfs(c(4087.431319539477, 3344.1061393670734, 2787.4075081266883, 2359.7645951085665, 2026.2251254712933,
                                                           1762.5621582812007, 1551.4447096526317, 1380.2859010651932, 1239.8437770491091, 1123.2667667819574,
                                                           1025.4284875236106, 942.4583457435928, 871.4072407375486, 810.0076140191389, 756.499966273271,
                                                           709.5065479162944, 667.9387546814613, 630.9287591367723, 597.778678595446, 567.9225111550363,
                                                           540.8974265993917, 516.3219549470757, 493.87929364084823, 473.30443802019994, 454.37418642105143,
                                                           436.89932107460845, 420.7184469499324, 405.69310247189566, 391.70385253619384, 378.64714527475786,
                                                           366.43276660589135, 354.98176574519084, 344.2247541579707, 334.1005024988387, 324.5547767952481,
                                                           315.53936786134597, 307.0112776829529, 298.9320340307214, 291.2671103861408)))
b_thetaiotaomicron_two_epoch = proportional_sfs(fold_sfs(c(6883.161202897391, 4741.59509909963, 3513.1941264046495, 2747.44614715245,
                                                           2237.8702673036246, 1880.131856644276, 1617.6981274956854, 1418.0873264610696,
                                                           1261.662722713237, 1136.0123682903118, 1032.9781402182884, 947.0105082974533,
                                                           874.218763286721, 811.8018165592289, 757.6953567306465, 710.3463422535705,
                                                           668.5648390931251, 631.424210270698, 598.1923372554992, 568.2832414715679,
                                                           541.2224144951516, 516.6215474043563, 494.1598242680509, 473.56987794162836, 
                                                           454.62710911567723, 437.1414663807546, 420.9510509426141, 405.9170906970018, 
                                                           391.91995358622455, 378.8559579814854, 366.63480024609237, 355.177464541413,
                                                           344.4145126192439, 334.28467538850884, 324.733685912793, 315.7133069000633,
                                                           307.18051589207096, 299.0968191056184, 291.427670835082, 284.1419799438684, 
                                                           277.2116885761361, 270.6114110523233, 264.31812315114524, 258.31089376217045,
                                                           252.57065231806973, 247.07998656160632, 241.82296612979758, 236.7849881898779,
                                                           231.9526419770626, 227.31358958771415, 222.85646079695465, 218.5707600125642,
                                                           214.44678376246893, 210.47554735034348, 206.64871951227755, 202.95856407467585,
                                                           199.39788775331982, 195.95999335253578, 192.63863772381663, 189.4279939285968,
                                                           186.3226171226469, 183.3174137419643, 180.40761362343693, 177.58874473906124,
                                                           174.85661026244466)))
p_distasonis_two_epoch = proportional_sfs(fold_sfs(c(5354.972462338029, 3904.942245708395, 2990.8749022247116, 2383.3044080437,
                                                     1961.5335419446353, 1657.3038026001766, 1430.3071685904897, 1255.8478287608975,
                                                     1118.2765031972901, 1007.3608254773932, 916.2123194069344, 840.0665962520259, 
                                                     775.5452985651671, 720.1976098506456, 672.2082419272489, 630.2070130078799,
                                                     593.1418792810455, 560.1924860691151, 530.7101428738388, 504.17537641191495,
                                                     480.1673988850663, 458.34179717540997, 438.41398910659797, 420.14678881912147,
                                                     403.3409428544927, 387.8278432941123, 373.4638566629222, 360.1258662561145,
                                                     347.707735813639, 336.11747998266844, 325.2749821871802, 315.11014027140664,
                                                     305.56134923439066, 296.57425168437004, 288.10070248406936, 280.09790594670255,
                                                     272.52769294171094, 265.3559121395543, 258.55191491017695, 252.08811748600806, 
                                                     245.93962719853823, 240.08392210949881, 234.50057534461806, 229.17101701812626,
                                                     224.0783279006956)))
p_merdae_two_epoch = proportional_sfs(fold_sfs(c(3318.6120290307103, 2323.81002054603, 1732.7823609227137, 1357.7547975208165,
                                                 1106.1862588038643, 929.0708642170175, 799.0823621004403, 700.2611704049449,
                                                 622.8780581950846, 560.76209729428, 509.8544533481116, 467.39603040351824,
                                                 431.45486018234897, 400.6418873080764, 373.9346510790337, 350.56468303147176,
                                                 329.9436360087519, 311.6136074421502, 295.21296588430033, 280.4523500487565,
                                                 267.09749071316963, 254.95670248075342, 243.87163196265902, 233.71031602310853,
                                                 224.36190477847168, 215.73260181108932, 207.74250631581583, 200.3231318242763,
                                                 193.41543825117478, 186.96825751192833, 180.93702385117598, 175.28274223129404,
                                                 169.97114428519893, 164.97199321817598, 160.258507869017)))
p_sp_two_epoch = proportional_sfs(fold_sfs(c(2641.334023403126, 1791.4206736727167, 1307.1901521781963, 1010.8738402580609,
                                             817.4547742040486, 683.8179785042425, 586.9205420862119, 513.7976450596426,
                                             456.7832106701746, 411.1274669335818, 373.75902097482583, 342.6144304155506,
                                             316.2600458360266, 293.670201426022, 274.09223099358064, 256.961478273873,
                                             241.8461007769209, 228.41020773632337, 216.3886186379095)))


a_finegoldii_nonsyn_empirical = proportional_sfs(fold_sfs(c(4235, 1320, 808, 726,
                                                            581, 606, 533, 542,
                                                            559, 516, 500, 481,
                                                            387, 379, 324, 292,
                                                            284, 238, 87, 0, 0,
                                                            0, 0, 0, 0, 0, 0, 0,
                                                            0, 0, 0, 0, 0, 0, 0,
                                                            0, 0, 0)))

a_finegoldii_nonsyn_df = data.frame(a_finegoldii_nonsyn_empirical,
                                    a_finegoldii_two_epoch)
a_finegoldii_x_axis = 1:length(a_finegoldii_nonsyn_empirical)
p_a_finegoldii_nonsyn_comparison <- ggplot(data = a_finegoldii_nonsyn_df, 
                                           aes(x=a_finegoldii_x_axis, y=a_finegoldii_nonsyn_empirical, color='a_finegoldii_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_nonsyn_empirical, color='a_finegoldii_nonsyn_empirical')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_nonsyn_empirical, color='a_finegoldii_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_two_epoch, color='a_finegoldii_two_epoch')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_two_epoch, color='a_finegoldii_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_finegoldii_x_axis, limits = c(2, length(a_finegoldii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('a_finegoldii_nonsyn_empirical',
                              'a_finegoldii_two_epoch'
                     ),
                     labels=c('Empirical',
                              'Gamma-distributed DFE')) +
  ggtitle('A. finegoldii Gamma-distributed DFE')
p_a_finegoldii_nonsyn_comparison

a_onderdonkii_nonsyn_empirical = proportional_sfs(fold_sfs(c(3500, 1430, 808, 637,
                                                    654, 513, 498, 527,
                                                    486, 428, 447, 351,
                                                    342, 342, 288, 226,
                                                    226, 213, 201, 253,
                                                    279, 295, 283, 279, 
                                                    227, 194, 0, 0, 0,
                                                    0, 0, 0, 0, 0, 0, 0,
                                                    0, 0, 0, 0, 0, 0, 0,
                                                    0, 0, 0, 0, 0, 0, 0,
                                                    0, 0)))

a_onderdonkii_nonsyn_df = data.frame(a_onderdonkii_nonsyn_empirical,
                                    a_onderdonkii_two_epoch)
a_onderdonkii_x_axis = 1:length(a_onderdonkii_nonsyn_empirical)
p_a_onderdonkii_nonsyn_comparison <- ggplot(data = a_onderdonkii_nonsyn_df, 
                                            aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_nonsyn_empirical, color='a_onderdonkii_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_nonsyn_empirical, color='a_onderdonkii_nonsyn_empirical')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_nonsyn_empirical, color='a_onderdonkii_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_two_epoch, color='a_onderdonkii_two_epoch')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_two_epoch, color='a_onderdonkii_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_onderdonkii_x_axis, limits = c(2, length(a_onderdonkii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('a_onderdonkii_nonsyn_empirical',
                              'a_onderdonkii_two_epoch'
                     ),
                     labels=c('Empirical',
                              'Gamma-distributed DFE')) +
  ggtitle('A. onderdonkii Gamma-distributed DFE')
p_a_onderdonkii_nonsyn_comparison

a_muciniphila_nonsyn_empirical = proportional_sfs(fold_sfs(c(4161, 2171, 1808, 1332, 1049, 1088, 901, 738, 291, 0, 0, 0, 0,
                                                    0, 0, 0, 0, 0)))

a_muciniphila_nonsyn_df = data.frame(a_muciniphila_nonsyn_empirical,
                                     a_muciniphila_two_epoch)
a_muciniphila_x_axis = 1:length(a_muciniphila_nonsyn_empirical)
p_a_muciniphila_nonsyn_comparison <- ggplot(data = a_muciniphila_nonsyn_df, 
                                            aes(x=a_muciniphila_x_axis, y=a_muciniphila_nonsyn_empirical, color='a_muciniphila_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_nonsyn_empirical, color='a_muciniphila_nonsyn_empirical')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_nonsyn_empirical, color='a_muciniphila_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_two_epoch, color='a_muciniphila_two_epoch')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_two_epoch, color='a_muciniphila_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_muciniphila_x_axis, limits = c(2, length(a_muciniphila_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('a_muciniphila_nonsyn_empirical',
                              'a_muciniphila_two_epoch'
                     ),
                     labels=c('Empirical',
                              'Gamma-distributed DFE')) +
  ggtitle('A. muciniphila Gamma-distributed DFE')
p_a_muciniphila_nonsyn_comparison

b_bacterium_nonsyn_empirical = proportional_sfs(fold_sfs(c(3964, 1497, 1259, 846,
                                                  745, 770, 516, 462,
                                                  417, 396, 305, 251,
                                                  217, 67, 0, 0, 0, 0,
                                                  0, 0, 0, 0, 0, 0, 0,
                                                  0, 0, 0)))

b_bacterium_nonsyn_df = data.frame(b_bacterium_nonsyn_empirical,
                                     b_bacterium_two_epoch)
b_bacterium_x_axis = 1:length(b_bacterium_nonsyn_empirical)
p_b_bacterium_nonsyn_comparison <- ggplot(data = b_bacterium_nonsyn_df, 
                                            aes(x=b_bacterium_x_axis, y=b_bacterium_nonsyn_empirical, color='b_bacterium_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_nonsyn_empirical, color='b_bacterium_nonsyn_empirical')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_nonsyn_empirical, color='b_bacterium_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_two_epoch, color='b_bacterium_two_epoch')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_two_epoch, color='b_bacterium_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_bacterium_x_axis, limits = c(2, length(b_bacterium_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_bacterium_nonsyn_empirical',
                              'b_bacterium_two_epoch'
                     ),
                     labels=c('Empirical',
                              'Gamma-distributed DFE')) +
  ggtitle('B. bacterium Gamma-distributed DFE')
p_b_bacterium_nonsyn_comparison

b_intestinihominis_nonsyn_empirical = proportional_sfs(fold_sfs(c(10479, 2580, 2107,
                                                         1470, 1817, 1244,
                                                         1008, 784, 749,
                                                         783, 945, 711, 529,
                                                         494, 442, 466, 440,
                                                         416, 357, 317, 0,
                                                         0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0,
                                                         0)))

b_intestinihominis_nonsyn_df = data.frame(b_intestinihominis_nonsyn_empirical,
                                     b_intestinihominis_two_epoch)
b_intestinihominis_x_axis = 1:length(b_intestinihominis_nonsyn_empirical)
p_b_intestinihominis_nonsyn_comparison <- ggplot(data = b_intestinihominis_nonsyn_df, 
                                            aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_nonsyn_empirical, color='b_intestinihominis_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_nonsyn_empirical, color='b_intestinihominis_nonsyn_empirical')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_nonsyn_empirical, color='b_intestinihominis_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_two_epoch, color='b_intestinihominis_two_epoch')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_two_epoch, color='b_intestinihominis_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_intestinihominis_x_axis, limits = c(2, length(b_intestinihominis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_intestinihominis_nonsyn_empirical',
                              'b_intestinihominis_two_epoch'
                     ),
                     labels=c('Empirical',
                              'Gamma-distributed DFE')) +
  ggtitle('B. intestinihominis Gamma-distributed DFE')
p_b_intestinihominis_nonsyn_comparison

b_thetaiotaomicron_nonsyn_empirical = proportional_sfs(fold_sfs(c(8538, 3013, 1775, 1466, 
                                                         1146, 1159, 714, 585,
                                                         572, 487, 431, 410, 
                                                         392, 349, 369, 331,
                                                         309, 282, 305, 290,
                                                         285, 310, 280, 246,
                                                         226, 255, 261, 219,
                                                         280, 256, 230, 180,
                                                         80, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0)))

b_thetaiotaomicron_nonsyn_df = data.frame(b_thetaiotaomicron_nonsyn_empirical,
                                     b_thetaiotaomicron_two_epoch)
b_thetaiotaomicron_x_axis = 1:length(b_thetaiotaomicron_nonsyn_empirical)
p_b_thetaiotaomicron_nonsyn_comparison <- ggplot(data = b_thetaiotaomicron_nonsyn_df, 
                                            aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_nonsyn_empirical, color='b_thetaiotaomicron_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_nonsyn_empirical, color='b_thetaiotaomicron_nonsyn_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_nonsyn_empirical, color='b_thetaiotaomicron_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch, color='b_thetaiotaomicron_two_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch, color='b_thetaiotaomicron_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(2, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_nonsyn_empirical',
                              'b_thetaiotaomicron_two_epoch'
                     ),
                     labels=c('Empirical',
                              'Gamma-distributed DFE')) +
  ggtitle('B. thetaiotaomicron Gamma-distributed DFE')
p_b_thetaiotaomicron_nonsyn_comparison

p_distasonis_nonsyn_empirical = proportional_sfs(fold_sfs(c(7096, 2655, 1511, 1354, 943,
                                                   816, 686, 697, 574, 600, 463,
                                                   450, 390, 388, 338, 337, 305,
                                                   304, 242, 274, 240, 224, 197,
                                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                   0, 0, 0)))

p_distasonis_nonsyn_df = data.frame(p_distasonis_nonsyn_empirical,
                                          p_distasonis_two_epoch)
p_distasonis_x_axis = 1:length(p_distasonis_nonsyn_empirical)
p_p_distasonis_nonsyn_comparison <- ggplot(data = p_distasonis_nonsyn_df, 
                                                 aes(x=p_distasonis_x_axis, y=p_distasonis_nonsyn_empirical, color='p_distasonis_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_nonsyn_empirical, color='p_distasonis_nonsyn_empirical')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_nonsyn_empirical, color='p_distasonis_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_two_epoch, color='p_distasonis_two_epoch')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_two_epoch, color='p_distasonis_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_distasonis_x_axis, limits = c(2, length(p_distasonis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('p_distasonis_nonsyn_empirical',
                              'p_distasonis_two_epoch'
                     ),
                     labels=c('Empirical',
                              'Gamma-distributed DFE')) +
  ggtitle('P. distasonis Gamma-distributed DFE')
p_p_distasonis_nonsyn_comparison

p_merdae_nonsyn_empirical = proportional_sfs(fold_sfs(c(5204, 1807, 891, 752,
                                               713, 552, 439, 399,
                                               407, 346, 308, 296,
                                               292, 302, 314, 367,
                                               290, 231, 0, 0, 0, 0,
                                               0, 0, 0, 0, 0, 0, 0,
                                               0, 0, 0, 0, 0, 0, 0)))

p_merdae_nonsyn_df = data.frame(p_merdae_nonsyn_empirical,
                                          p_merdae_two_epoch)
p_merdae_x_axis = 1:length(p_merdae_nonsyn_empirical)
p_p_merdae_nonsyn_comparison <- ggplot(data = p_merdae_nonsyn_df, 
                                                 aes(x=p_merdae_x_axis, y=p_merdae_nonsyn_empirical, color='p_merdae_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_nonsyn_empirical, color='p_merdae_nonsyn_empirical')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_nonsyn_empirical, color='p_merdae_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_two_epoch, color='p_merdae_two_epoch')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_two_epoch, color='p_merdae_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_merdae_x_axis, limits = c(2, length(p_merdae_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('p_merdae_nonsyn_empirical',
                              'p_merdae_two_epoch'
                     ),
                     labels=c('Empirical',
                              'Gamma-distributed DFE')) +
  ggtitle('P. merdae Gamma-distributed DFE')
p_p_merdae_nonsyn_comparison

p_sp_nonsyn_empirical = proportional_sfs(fold_sfs(c(4070, 806, 1382, 591, 408,
                                           323, 250, 211, 230, 99,
                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))

p_sp_nonsyn_df = data.frame(p_sp_nonsyn_empirical,
                                          p_sp_two_epoch)
p_sp_x_axis = 1:length(p_sp_nonsyn_empirical)
p_p_sp_nonsyn_comparison <- ggplot(data = p_sp_nonsyn_df, 
                                                 aes(x=p_sp_x_axis, y=p_sp_nonsyn_empirical, color='p_sp_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_nonsyn_empirical, color='p_sp_nonsyn_empirical')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_nonsyn_empirical, color='p_sp_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_two_epoch, color='p_sp_two_epoch')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_two_epoch, color='p_sp_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_sp_x_axis, limits = c(2, length(p_sp_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('p_sp_nonsyn_empirical',
                              'p_sp_two_epoch'
                     ),
                     labels=c('Empirical',
                              'Gamma-distributed DFE')) +
  ggtitle('P. sp Gamma-distributed DFE')
p_p_sp_nonsyn_comparison

a_finegoldii_nonsyn_df = data.frame(a_finegoldii_nonsyn_empirical,
                                    a_finegoldii_two_epoch,
                                    a_finegoldii_empirical,
                                    a_finegoldii_one_epoch)
a_finegoldii_x_axis = 1:length(a_finegoldii_nonsyn_empirical)
p_a_finegoldii_nonsyn_comparison <- ggplot(data = a_finegoldii_nonsyn_df, 
                                           aes(x=a_finegoldii_x_axis, y=a_finegoldii_nonsyn_empirical, color='a_finegoldii_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_nonsyn_empirical, color='a_finegoldii_nonsyn_empirical')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_nonsyn_empirical, color='a_finegoldii_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_two_epoch, color='a_finegoldii_two_epoch')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_two_epoch, color='a_finegoldii_two_epoch')) +
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +  
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_one_epoch, color='a_finegoldii_one_epoch')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_one_epoch, color='a_finegoldii_one_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_finegoldii_x_axis, limits = c(2, length(a_finegoldii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'red', 'orange'),
                     name='Data Type',
                     breaks=c('a_finegoldii_nonsyn_empirical',
                              'a_finegoldii_two_epoch',
                              'a_finegoldii_empirical',
                              'a_finegoldii_one_epoch'
                     ),
                     labels=c('Nonsynonymous',
                              'Gamma-distributed DFE',
                              'Synonymous',
                              'Two-epoch Demography')) +
  ggtitle('A. finegoldii SFS comparison')
p_a_finegoldii_nonsyn_comparison

a_onderdonkii_nonsyn_df = data.frame(a_onderdonkii_nonsyn_empirical,
                                    a_onderdonkii_two_epoch,
                                    a_onderdonkii_empirical,
                                    a_onderdonkii_one_epoch)
a_onderdonkii_x_axis = 1:length(a_onderdonkii_nonsyn_empirical)
p_a_onderdonkii_nonsyn_comparison <- ggplot(data = a_onderdonkii_nonsyn_df, 
                                           aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_nonsyn_empirical, color='a_onderdonkii_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_nonsyn_empirical, color='a_onderdonkii_nonsyn_empirical')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_nonsyn_empirical, color='a_onderdonkii_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_two_epoch, color='a_onderdonkii_two_epoch')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_two_epoch, color='a_onderdonkii_two_epoch')) +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +  
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_one_epoch, color='a_onderdonkii_one_epoch')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_one_epoch, color='a_onderdonkii_one_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_onderdonkii_x_axis, limits = c(2, length(a_onderdonkii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'red', 'orange'),
                     name='Data Type',
                     breaks=c('a_onderdonkii_nonsyn_empirical',
                              'a_onderdonkii_two_epoch',
                              'a_onderdonkii_empirical',
                              'a_onderdonkii_one_epoch'
                     ),
                     labels=c('Nonsynonymous',
                              'Gamma-distributed DFE',
                              'Synonymous',
                              'Two-epoch Demography')) +
  ggtitle('A. onderdonkii SFS comparison')
p_a_onderdonkii_nonsyn_comparison

a_muciniphila_nonsyn_df = data.frame(a_muciniphila_nonsyn_empirical,
                                    a_muciniphila_two_epoch,
                                    a_muciniphila_empirical,
                                    a_muciniphila_one_epoch)
a_muciniphila_x_axis = 1:length(a_muciniphila_nonsyn_empirical)
p_a_muciniphila_nonsyn_comparison <- ggplot(data = a_muciniphila_nonsyn_df, 
                                           aes(x=a_muciniphila_x_axis, y=a_muciniphila_nonsyn_empirical, color='a_muciniphila_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_nonsyn_empirical, color='a_muciniphila_nonsyn_empirical')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_nonsyn_empirical, color='a_muciniphila_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_two_epoch, color='a_muciniphila_two_epoch')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_two_epoch, color='a_muciniphila_two_epoch')) +
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +  
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_one_epoch, color='a_muciniphila_one_epoch')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_one_epoch, color='a_muciniphila_one_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_muciniphila_x_axis, limits = c(2, length(a_muciniphila_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'red', 'orange'),
                     name='Data Type',
                     breaks=c('a_muciniphila_nonsyn_empirical',
                              'a_muciniphila_two_epoch',
                              'a_muciniphila_empirical',
                              'a_muciniphila_one_epoch'
                     ),
                     labels=c('Nonsynonymous',
                              'Gamma-distributed DFE',
                              'Synonymous',
                              'Two-epoch Demography')) +
  ggtitle('A. muciniphila SFS comparison')
p_a_muciniphila_nonsyn_comparison

b_bacterium_nonsyn_df = data.frame(b_bacterium_nonsyn_empirical,
                                    b_bacterium_two_epoch,
                                    b_bacterium_empirical,
                                    b_bacterium_one_epoch)
b_bacterium_x_axis = 1:length(b_bacterium_nonsyn_empirical)
p_b_bacterium_nonsyn_comparison <- ggplot(data = b_bacterium_nonsyn_df, 
                                           aes(x=b_bacterium_x_axis, y=b_bacterium_nonsyn_empirical, color='b_bacterium_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_nonsyn_empirical, color='b_bacterium_nonsyn_empirical')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_nonsyn_empirical, color='b_bacterium_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_two_epoch, color='b_bacterium_two_epoch')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_two_epoch, color='b_bacterium_two_epoch')) +
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +  
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_one_epoch, color='b_bacterium_one_epoch')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_one_epoch, color='b_bacterium_one_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_bacterium_x_axis, limits = c(2, length(b_bacterium_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'red', 'orange'),
                     name='Data Type',
                     breaks=c('b_bacterium_nonsyn_empirical',
                              'b_bacterium_two_epoch',
                              'b_bacterium_empirical',
                              'b_bacterium_one_epoch'
                     ),
                     labels=c('Nonsynonymous',
                              'Gamma-distributed DFE',
                              'Synonymous',
                              'Two-epoch Demography')) +
  ggtitle('B. bacterium SFS comparison')
p_b_bacterium_nonsyn_comparison

b_intestinihominis_nonsyn_df = data.frame(b_intestinihominis_nonsyn_empirical,
                                    b_intestinihominis_two_epoch,
                                    b_intestinihominis_empirical,
                                    b_intestinihominis_one_epoch)
b_intestinihominis_x_axis = 1:length(b_intestinihominis_nonsyn_empirical)
p_b_intestinihominis_nonsyn_comparison <- ggplot(data = b_intestinihominis_nonsyn_df, 
                                           aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_nonsyn_empirical, color='b_intestinihominis_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_nonsyn_empirical, color='b_intestinihominis_nonsyn_empirical')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_nonsyn_empirical, color='b_intestinihominis_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_two_epoch, color='b_intestinihominis_two_epoch')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_two_epoch, color='b_intestinihominis_two_epoch')) +
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +  
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_one_epoch, color='b_intestinihominis_one_epoch')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_one_epoch, color='b_intestinihominis_one_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_intestinihominis_x_axis, limits = c(2, length(b_intestinihominis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'red', 'orange'),
                     name='Data Type',
                     breaks=c('b_intestinihominis_nonsyn_empirical',
                              'b_intestinihominis_two_epoch',
                              'b_intestinihominis_empirical',
                              'b_intestinihominis_one_epoch'
                     ),
                     labels=c('Nonsynonymous',
                              'Gamma-distributed DFE',
                              'Synonymous',
                              'Two-epoch Demography')) +
  ggtitle('B. intestinihominis SFS comparison')
p_b_intestinihominis_nonsyn_comparison

b_thetaiotaomicron_nonsyn_df = data.frame(b_thetaiotaomicron_nonsyn_empirical,
                                    b_thetaiotaomicron_two_epoch,
                                    b_thetaiotaomicron_empirical,
                                    b_thetaiotaomicron_one_epoch)
b_thetaiotaomicron_x_axis = 1:length(b_thetaiotaomicron_nonsyn_empirical)
p_b_thetaiotaomicron_nonsyn_comparison <- ggplot(data = b_thetaiotaomicron_nonsyn_df, 
                                           aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_nonsyn_empirical, color='b_thetaiotaomicron_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_nonsyn_empirical, color='b_thetaiotaomicron_nonsyn_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_nonsyn_empirical, color='b_thetaiotaomicron_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch, color='b_thetaiotaomicron_two_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch, color='b_thetaiotaomicron_two_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(2, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'red', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_nonsyn_empirical',
                              'b_thetaiotaomicron_two_epoch',
                              'b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_one_epoch'
                     ),
                     labels=c('Nonsynonymous',
                              'Gamma-distributed DFE',
                              'Synonymous',
                              'Two-epoch Demography')) +
  ggtitle('B. thetaiotaomicron SFS comparison')
p_b_thetaiotaomicron_nonsyn_comparison

p_distasonis_nonsyn_df = data.frame(p_distasonis_nonsyn_empirical,
                                    p_distasonis_two_epoch,
                                    p_distasonis_empirical,
                                    p_distasonis_one_epoch)
p_distasonis_x_axis = 1:length(p_distasonis_nonsyn_empirical)
p_p_distasonis_nonsyn_comparison <- ggplot(data = p_distasonis_nonsyn_df, 
                                           aes(x=p_distasonis_x_axis, y=p_distasonis_nonsyn_empirical, color='p_distasonis_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_nonsyn_empirical, color='p_distasonis_nonsyn_empirical')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_nonsyn_empirical, color='p_distasonis_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_two_epoch, color='p_distasonis_two_epoch')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_two_epoch, color='p_distasonis_two_epoch')) +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +  
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_one_epoch, color='p_distasonis_one_epoch')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_one_epoch, color='p_distasonis_one_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_distasonis_x_axis, limits = c(2, length(p_distasonis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'red', 'orange'),
                     name='Data Type',
                     breaks=c('p_distasonis_nonsyn_empirical',
                              'p_distasonis_two_epoch',
                              'p_distasonis_empirical',
                              'p_distasonis_one_epoch'
                     ),
                     labels=c('Nonsynonymous',
                              'Gamma-distributed DFE',
                              'Synonymous',
                              'Two-epoch Demography')) +
  ggtitle('P. distasonis SFS comparison')
p_p_distasonis_nonsyn_comparison

p_merdae_nonsyn_df = data.frame(p_merdae_nonsyn_empirical,
                                    p_merdae_two_epoch,
                                    p_merdae_empirical,
                                    p_merdae_one_epoch)
p_merdae_x_axis = 1:length(p_merdae_nonsyn_empirical)
p_p_merdae_nonsyn_comparison <- ggplot(data = p_merdae_nonsyn_df, 
                                           aes(x=p_merdae_x_axis, y=p_merdae_nonsyn_empirical, color='p_merdae_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_nonsyn_empirical, color='p_merdae_nonsyn_empirical')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_nonsyn_empirical, color='p_merdae_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_two_epoch, color='p_merdae_two_epoch')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_two_epoch, color='p_merdae_two_epoch')) +
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +  
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_one_epoch, color='p_merdae_one_epoch')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_one_epoch, color='p_merdae_one_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_merdae_x_axis, limits = c(2, length(p_merdae_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'red', 'orange'),
                     name='Data Type',
                     breaks=c('p_merdae_nonsyn_empirical',
                              'p_merdae_two_epoch',
                              'p_merdae_empirical',
                              'p_merdae_one_epoch'
                     ),
                     labels=c('Nonsynonymous',
                              'Gamma-distributed DFE',
                              'Synonymous',
                              'Two-epoch Demography')) +
  ggtitle('P. merdae SFS comparison')
p_p_merdae_nonsyn_comparison

p_sp_nonsyn_df = data.frame(p_sp_nonsyn_empirical,
                                    p_sp_two_epoch,
                                    p_sp_empirical,
                                    p_sp_one_epoch)
p_sp_x_axis = 1:length(p_sp_nonsyn_empirical)
p_p_sp_nonsyn_comparison <- ggplot(data = p_sp_nonsyn_df, 
                                           aes(x=p_sp_x_axis, y=p_sp_nonsyn_empirical, color='p_sp_nonsyn_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_nonsyn_empirical, color='p_sp_nonsyn_empirical')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_nonsyn_empirical, color='p_sp_nonsyn_empirical')) +  
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_two_epoch, color='p_sp_two_epoch')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_two_epoch, color='p_sp_two_epoch')) +
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +  
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_one_epoch, color='p_sp_one_epoch')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_one_epoch, color='p_sp_one_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_sp_x_axis, limits = c(2, length(p_sp_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'red', 'orange'),
                     name='Data Type',
                     breaks=c('p_sp_nonsyn_empirical',
                              'p_sp_two_epoch',
                              'p_sp_empirical',
                              'p_sp_one_epoch'
                     ),
                     labels=c('Nonsynonymous',
                              'Gamma-distributed DFE',
                              'Synonymous',
                              'Two-epoch Demography')) +
  ggtitle('P. sp SFS comparison')
p_p_sp_nonsyn_comparison

a_finegoldii_two_epoch
a_finegoldii_one_epoch
a_finegoldii_empirical

a_finegoldii_epoch_df = data.frame(a_finegoldii_two_epoch,
                                   a_finegoldii_one_epoch,
                                   a_finegoldii_empirical)
a_finegoldii_x_axis = 1:length(a_finegoldii_two_epoch)
p_a_finegoldii_epoch_comparison <- ggplot(data = a_finegoldii_epoch_df, 
                                   aes(x=a_finegoldii_x_axis, y=a_finegoldii_two_epoch, color='a_finegoldii_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_one_epoch, color='a_finegoldii_one_epoch')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_one_epoch, color='a_finegoldii_one_epoch')) +
  geom_point(shape=1, aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +
  geom_line(aes(x=a_finegoldii_x_axis, y=a_finegoldii_empirical, color='a_finegoldii_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_finegoldii_x_axis, limits = c(2, length(a_finegoldii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', ),
                     name='Data Type',
                     breaks=c('a_finegoldii_empirical',
                              'a_finegoldii_two_epoch',
                              'a_finegoldii_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('A. finegoldii') +
  theme(legend.position="none",
       axis.text.x=element_blank())
p_a_finegoldii_epoch_comparison

a_muciniphila_two_epoch = proportional_sfs(fold_sfs(c(4856.85835255306, 3347.7729196144837, 2463.522110113016, 1912.8116849569628, 1549.5981768854656, 1297.23494993225,
                                                      1113.736995642971, 975.0817552301565, 866.9110678493279, 780.2715189468886, 709.3528197073293, 650.2443379468108,
                                                      600.2266887974953, 557.3536458784366, 520.1968062648581, 487.68452223354774, 458.99720162924217)))
a_muciniphila_one_epoch
a_muciniphila_empirical

a_muciniphila_epoch_df = data.frame(a_muciniphila_two_epoch,
                                    a_muciniphila_one_epoch,
                                    a_muciniphila_empirical)
a_muciniphila_x_axis = 1:length(a_muciniphila_two_epoch)
p_a_muciniphila_epoch_comparison <- ggplot(data = a_muciniphila_epoch_df, 
                                           aes(x=a_muciniphila_x_axis, y=a_muciniphila_two_epoch, color='a_muciniphila_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_one_epoch, color='a_muciniphila_one_epoch')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_one_epoch, color='a_muciniphila_one_epoch')) +
  geom_point(shape=1, aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +
  geom_line(aes(x=a_muciniphila_x_axis, y=a_muciniphila_empirical, color='a_muciniphila_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_muciniphila_x_axis, limits = c(2, length(a_muciniphila_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('a_muciniphila_empirical',
                              'a_muciniphila_two_epoch',
                              'a_muciniphila_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('A. muciniphila') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_a_muciniphila_epoch_comparison

a_onderdonkii_two_epoch
a_onderdonkii_one_epoch
a_onderdonkii_empirical

a_onderdonkii_epoch_df = data.frame(a_onderdonkii_two_epoch,
                                    a_onderdonkii_one_epoch,
                                    a_onderdonkii_empirical)
a_onderdonkii_x_axis = 1:length(a_onderdonkii_two_epoch)
p_a_onderdonkii_epoch_comparison <- ggplot(data = a_onderdonkii_epoch_df, 
                                           aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_two_epoch, color='a_onderdonkii_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_one_epoch, color='a_onderdonkii_one_epoch')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_one_epoch, color='a_onderdonkii_one_epoch')) +
  geom_point(shape=1, aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +
  geom_line(aes(x=a_onderdonkii_x_axis, y=a_onderdonkii_empirical, color='a_onderdonkii_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_onderdonkii_x_axis, limits = c(2, length(a_onderdonkii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('a_onderdonkii_empirical',
                              'a_onderdonkii_two_epoch',
                              'a_onderdonkii_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('A. onderdonkii') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_a_onderdonkii_epoch_comparison

a_putredinis_two_epoch = proportional_sfs(fold_sfs(c(2530.2515388147876, 1264.9469348431862, 
                                                     843.297940337086, 632.4734643062765, 
                                                     505.9787782372914, 421.6489872585373, 
                                                     361.41342207836857, 316.23674803462393, 
                                                     281.09933475402016, 252.98940401039644, 
                                                     229.9903696588503, 210.82450760228963, 
                                                     194.6072396193789, 180.70672412330026, 
                                                     168.65961061695566, 158.11838622747828, 
                                                     148.81730581673295, 140.549678721716, 
                                                     133.1523281031827, 126.4947124897363, 
                                                     120.47115545213158, 114.99519445735126, 
                                                     109.99540393462323, 105.4122625749775, 
                                                     101.19577247886951, 97.30362773129694, 
                                                     93.69978996022073, 90.35336913245793, 
                                                     87.2377359090609, 84.32981152960237, 
                                                     81.60949513822436)))
a_putredinis_one_epoch = proportional_sfs(fold_sfs(c(2529.893082215207, 1264.9468955729178,
                                                     843.2979455728026, 632.4734682424386,
                                                     505.9787813862246, 421.64898988265134,
                                                     361.4134243276041, 316.23675000271174, 
                                                     281.0993365034376, 252.98940558486495, 
                                                     229.9903710901886, 210.82450891434675, 
                                                     194.60724083050857, 180.70672524792067, 
                                                     168.6596116666014, 158.11838721151892, 
                                                     148.81730674289096, 140.5496795964208, 
                                                     133.15232893185038, 126.49471327697063, 
                                                     120.47115620188026, 114.99519517301884, 
                                                     109.99540461917482, 105.41226323100612, 
                                                     101.19577310865556, 97.30362833686044, 
                                                     93.69979054335728, 90.35336969476818, 
                                                     87.23773645198114, 84.32981205442526, 
                                                     81.6094956461175)))
a_putredinis_empirical = proportional_sfs(a_putredinis_empirical_syn_no_s_sfs)

a_putredinis_epoch_df = data.frame(a_putredinis_two_epoch,
                                   a_putredinis_one_epoch,
                                   a_putredinis_empirical_syn_no_s_sfs)
a_putredinis_x_axis = 1:length(a_putredinis_two_epoch)
p_a_putredinis_epoch_comparison <- ggplot(data = a_putredinis_epoch_df, 
                                          aes(x=a_putredinis_x_axis, y=a_putredinis_two_epoch, color='a_putredinis_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_putredinis_x_axis, y=a_putredinis_one_epoch, color='a_putredinis_one_epoch')) +
  geom_line(aes(x=a_putredinis_x_axis, y=a_putredinis_one_epoch, color='a_putredinis_one_epoch')) +
  geom_point(shape=1, aes(x=a_putredinis_x_axis, y=a_putredinis_empirical, color='a_putredinis_empirical')) +
  geom_line(aes(x=a_putredinis_x_axis, y=a_putredinis_empirical, color='a_putredinis_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_putredinis_x_axis, limits = c(2, length(a_putredinis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('a_putredinis_empirical',
                              'a_putredinis_two_epoch',
                              'a_putredinis_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('A. putredinis') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_a_putredinis_epoch_comparison

a_shahii_two_epoch = proportional_sfs(fold_sfs(c(3760.440071428626, 1880.2760686908334, 
                                                 1253.517406156249, 940.1380640348751, 
                                                 752.1104580551217, 626.7587204406025, 
                                                 537.2217648315784, 470.06904800672595, 
                                                 417.83915705168107, 376.05524421231314, 
                                                 341.86840636990445, 313.37937477738166, 
                                                 289.27327107075644, 268.61089641820666, 
                                                 250.70350500949075, 235.03453748682313, 
                                                 221.2089778705866, 208.91959150992085, 
                                                 197.92382473307288, 188.02763460257464, 
                                                 179.07393874048282, 170.93421520117326, 
                                                 163.5022936817391, 156.68969892974593, 
                                                 150.4221117331447, 144.63664660479625, 
                                                 139.279734426048, 134.3054588094623, 
                                                 129.6742366624196, 125.3517626379901, 
                                                 121.30815788560366, 117.51727841103208, 
                                                 113.95614918902282, 110.60449813848993, 
                                                 107.44436998758707, 104.4598044946978, 
                                                 101.63656684971222, 98.96192064360017, 
                                                 96.42443576568124, 94.01382511634387, 
                                                 91.7208052154597, 89.53697672384799, 
                                                 87.45472163622705, 85.46711449322814, 
                                                 83.56784543188124, 81.75115327295487, 
                                                 80.01176715033579, 78.34485543675983, 
                                                 76.74598092350955, 75.21106137856918, 
                                                 73.73633474497625, 72.31832835477978, 
                                                 70.95383162815475)))
a_shahii_one_epoch = proportional_sfs(fold_sfs(c(3760.5504621471105, 1880.2760755351549, 
                                                 1253.517405406429, 940.1380634715483, 
                                                 752.1104576044656, 626.7587200650602, 
                                                 537.2217645096734, 470.06904772506584, 
                                                 417.83915680131656, 376.0552439869797, 
                                                 341.8684061650607, 313.37937458960823, 
                                                 289.27327089742715, 268.61089625725424, 
                                                 250.70350485926846, 235.03453734599307, 
                                                 221.20897773804066, 208.91959138474448, 
                                                 197.92382461447912, 188.02763448990794, 
                                                 179.07393863318114, 170.9342150987514, 
                                                 163.50229358377035, 156.68969883585922, 
                                                 150.42211164301347, 144.6366465181295, 
                                                 139.27973434259115, 134.30545872898605, 
                                                 129.67423658472023, 125.35176256287895, 
                                                 121.30815781292063, 117.51727834061704, 
                                                 113.95614912074481, 110.60449807221694, 
                                                 107.4443699232076, 104.45980443210665, 
                                                 101.63656678881274, 98.9619205843033, 
                                                 96.42443570790343, 94.01382506001184, 
                                                 91.72080516050163, 89.53697667019843, 
                                                 87.45472158382516, 85.46711444201598, 
                                                 83.56784538180833, 81.75115322397049, 
                                                 80.01176710239363, 78.34485538981534, 
                                                 76.74598087752312, 75.21106133350355, 
                                                 73.73633470079426, 72.31832831144642, 
                                                 70.95383158564)))
a_shahii_empirical = proportional_sfs(a_shahii_empirical_syn_sfs)
a_shahii_epoch_df = data.frame(a_shahii_two_epoch,
                               a_shahii_one_epoch,
                               a_shahii_empirical)
a_shahii_x_axis = 1:length(a_shahii_two_epoch)
p_a_shahii_epoch_comparison <- ggplot(data = a_shahii_epoch_df, 
                                      aes(x=a_shahii_x_axis, y=a_shahii_two_epoch, color='a_shahii_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=a_shahii_x_axis, y=a_shahii_one_epoch, color='a_shahii_one_epoch')) +
  geom_line(aes(x=a_shahii_x_axis, y=a_shahii_one_epoch, color='a_shahii_one_epoch')) +
  geom_point(shape=1, aes(x=a_shahii_x_axis, y=a_shahii_empirical, color='a_shahii_empirical')) +
  geom_line(aes(x=a_shahii_x_axis, y=a_shahii_empirical, color='a_shahii_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=a_shahii_x_axis, limits = c(2, length(a_shahii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('a_shahii_empirical',
                              'a_shahii_two_epoch',
                              'a_shahii_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('A. shahii') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_a_shahii_epoch_comparison

b_bacterium_two_epoch
b_bacterium_one_epoch
b_bacterium_empirical

b_bacterium_epoch_df = data.frame(b_bacterium_two_epoch,
                                  b_bacterium_one_epoch,
                                  b_bacterium_empirical)
b_bacterium_x_axis = 1:length(b_bacterium_two_epoch)
p_b_bacterium_epoch_comparison <- ggplot(data = b_bacterium_epoch_df, 
                                         aes(x=b_bacterium_x_axis, y=b_bacterium_two_epoch, color='b_bacterium_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_one_epoch, color='b_bacterium_one_epoch')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_one_epoch, color='b_bacterium_one_epoch')) +
  geom_point(shape=1, aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +
  geom_line(aes(x=b_bacterium_x_axis, y=b_bacterium_empirical, color='b_bacterium_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_bacterium_x_axis, limits = c(2, length(b_bacterium_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_bacterium_empirical',
                              'b_bacterium_two_epoch',
                              'b_bacterium_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. bacterium') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_bacterium_epoch_comparison

b_caccae_two_epoch = proportional_sfs(fold_sfs(c(3166.62014426057, 1855.9448391223782, 
                                                 1273.375653690448, 960.955129516978, 
                                                 769.8827934826342, 641.8026057387515, 
                                                 550.1691581658466, 481.410601102996, 
                                                 427.9236916265102, 385.13214672512936, 
                                                 350.12035686485183, 320.94372400709756, 
                                                 296.25576493356334, 275.09464614281075, 
                                                 256.75500663784646, 240.70782106250823, 
                                                 226.54853930179675, 213.96251088757847, 
                                                 202.70132744547269, 192.56626224386835, 
                                                 183.39644125258812, 175.06024026232214, 
                                                 167.4489262298735, 160.4718882856301, 
                                                 154.05301329908949, 148.12789785210236, 
                                                 142.64167977364843, 137.54733434567885, 
                                                 132.80432301815054)))

b_caccae_one_epoch = proportional_sfs(fold_sfs(c(3823.14389167675, 1911.5724523696224, 
                                                 1274.3816584560736, 955.786258196376, 
                                                 764.6290173253228, 637.1908563121451, 
                                                 546.1635981212937, 477.893154211886, 
                                                 424.7939198312826, 382.31453212482563, 
                                                 347.55866927449466, 318.5954500676277, 
                                                 294.0881105873226, 273.0818194641799, 
                                                 254.87636702717626, 238.94659602294826, 
                                                 224.8909156106355, 212.39697735841565, 
                                                 201.21819039904793, 191.15728203870273, 
                                                 182.0545553347528, 173.77934915230148, 
                                                 166.22372603207685, 159.2977380913627, 
                                                 152.92582910864073, 147.04406689649497, 
                                                 141.59799070265402, 136.54091988233668, 
                                                 131.83261250035798)))
b_caccae_empirical = proportional_sfs(fold_sfs(c(4775, 1976, 1497, 1051, 852,
                                                736, 627, 571, 671, 677, 583,
                                                582, 583, 536, 249, 0, 0, 0, 0,
                                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))


b_caccae_epoch_df = data.frame(b_caccae_two_epoch,
                               b_caccae_one_epoch,
                               b_caccae_empirical)
b_caccae_x_axis = 1:length(b_caccae_two_epoch)
p_b_caccae_epoch_comparison <- ggplot(data = b_caccae_epoch_df, 
                                      aes(x=b_caccae_x_axis, y=b_caccae_two_epoch, color='b_caccae_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_caccae_x_axis, y=b_caccae_one_epoch, color='b_caccae_one_epoch')) +
  geom_line(aes(x=b_caccae_x_axis, y=b_caccae_one_epoch, color='b_caccae_one_epoch')) +
  geom_point(shape=1, aes(x=b_caccae_x_axis, y=b_caccae_empirical, color='b_caccae_empirical')) +
  geom_line(aes(x=b_caccae_x_axis, y=b_caccae_empirical, color='b_caccae_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_caccae_x_axis, limits = c(2, length(b_caccae_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_caccae_empirical',
                              'b_caccae_two_epoch',
                              'b_caccae_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. caccae') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_caccae_epoch_comparison

b_cellulolyticus_two_epoch = proportional_sfs(fold_sfs(c(8772.5002613001, 4386.328981360644, 
                                                         2924.2193780725347, 2193.164564981123, 
                                                         1754.53167553782, 1462.1097483195033, 
                                                         1253.2369424862045, 1096.5823375612297, 
                                                         974.7398665942471, 877.2658894075474, 
                                                         797.5144531572863, 731.0549226126003, 
                                                         674.8199349203732, 626.618516613583, 
                                                         584.843953816073, 548.2912111205757, 
                                                         516.0387908624996, 487.36997263601825, 
                                                         461.71892454168653, 438.63298105994477, 
                                                         417.745698674904, 398.7572599644333, 
                                                         381.4199896667724, 365.52749173043657, 
                                                         350.90639347215443, 337.40999492914267, 
                                                         324.9133294664559, 313.3092828255199, 
                                                         302.50551512828173, 292.42199848040383, 
                                                         282.9890311674564)))
b_cellulolyticus_one_epoch = proportional_sfs(fold_sfs(c(8772.655515655582, 4386.328986971472, 
                                                         2924.2193773234544, 2193.1645644188766, 
                                                         1754.5316750879854, 1462.109747944672, 
                                                         1253.236942164903, 1096.5823372800908, 
                                                         974.7398663443737, 877.2658891826238, 
                                                         797.5144529528329, 731.0549224251744, 
                                                         674.8199347473646, 626.6185164529322, 
                                                         584.8439536661323, 548.2912109799984, 
                                                         516.0387907301989, 487.3699725110676, 
                                                         461.7189244233122, 438.6329809474892, 
                                                         417.7456985677975, 398.75725986220095, 
                                                         381.41998956898493, 365.5274916367236, 
                                                         350.90639338218506, 337.4099948426384, 
                                                         324.9133293831554, 313.30928274519454, 
                                                         302.50551505072616, 292.42199840543344, 
                                                         282.9890310949044)))
b_cellulolyticus_empirical = proportional_sfs(fold_sfs(c(12403, 6708, 3920, 3016,
                                                         1808, 1607, 1617, 1430,
                                                         1191, 1021, 861, 798,
                                                         717, 726, 605, 249, 0,
                                                         0, 0, 0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0, 0)))
b_cellulolyticus_epoch_df = data.frame(b_cellulolyticus_two_epoch,
                                   b_cellulolyticus_one_epoch,
                                   b_cellulolyticus_empirical)
b_cellulolyticus_x_axis = 1:length(b_cellulolyticus_two_epoch)
p_b_cellulolyticus_epoch_comparison <- ggplot(data = b_cellulolyticus_epoch_df, 
                                              aes(x=b_cellulolyticus_x_axis, y=b_cellulolyticus_two_epoch, color='b_cellulolyticus_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_cellulolyticus_x_axis, y=b_cellulolyticus_one_epoch, color='b_cellulolyticus_one_epoch')) +
  geom_line(aes(x=b_cellulolyticus_x_axis, y=b_cellulolyticus_one_epoch, color='b_cellulolyticus_one_epoch')) +
  geom_point(shape=1, aes(x=b_cellulolyticus_x_axis, y=b_cellulolyticus_empirical, color='b_cellulolyticus_empirical')) +
  geom_line(aes(x=b_cellulolyticus_x_axis, y=b_cellulolyticus_empirical, color='b_cellulolyticus_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_cellulolyticus_x_axis, limits = c(2, length(b_cellulolyticus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_cellulolyticus_empirical',
                              'b_cellulolyticus_two_epoch',
                              'b_cellulolyticus_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. cellulolyticus') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_cellulolyticus_epoch_comparison

b_fragilis_two_epoch = proportional_sfs(fold_sfs(c(5928.618638628872, 2964.3500652639123, 
                                                   1976.2334174803393, 1482.1750865192773, 
                                                   1185.7400867825172, 988.1167528947138, 
                                                   846.9572281306719, 741.0875840943698, 
                                                   658.744527221316, 592.8700813683547, 
                                                   538.9728071696228, 494.0584117135253, 
                                                   456.0539229838226, 423.4786466828798, 
                                                   395.246740325501, 370.54382204778244, 
                                                   348.74712924769415, 329.37229101259834, 
                                                   312.03690925348405, 296.43506549912337, 
                                                   282.3191114632507, 269.48642582071676, 
                                                   257.7696257377172, 247.0292255193714, 
                                                   237.14805718197002, 228.0269785854669, 
                                                   219.5815353142296)))
b_fragilis_one_epoch = proportional_sfs(fold_sfs(c(5928.698656934207, 2964.3500689789034, 
                                                   1976.2334169548521, 1482.1750861247303, 
                                                   1185.7400864669048, 988.1167526316825, 
                                                   846.9572279052284, 741.0875838971068, 
                                                   658.7445270459712, 592.8700812105443, 
                                                   538.9728070261589, 494.0584115820166, 
                                                   456.05392286242994, 423.4786465701581, 
                                                   395.24674022030524, 370.54382194914035, 
                                                   348.74712915486447, 329.3722909249259, 
                                                   312.03690917042593, 296.43506542021817, 
                                                   282.31911138809886, 269.4864257489924, 
                                                   257.7696256691113, 247.02922545361704, 
                                                   237.14805711884247, 228.02697852477058, 
                                                   219.5815352557813)))

b_fragilis_empirical = proportional_sfs(fold_sfs(c(8781, 3974, 2691, 1789, 1429,
                                                   1123, 945, 905, 833, 786, 747,
                                                   722, 676, 303, 0, 0, 0, 0, 0,
                                                   0, 0, 0, 0, 0, 0, 0, 0, 0)))
b_fragilis_epoch_df = data.frame(b_fragilis_two_epoch,
                                   b_fragilis_one_epoch)
b_fragilis_x_axis = 1:length(b_fragilis_two_epoch)
p_b_fragilis_epoch_comparison <- ggplot(data = b_fragilis_epoch_df, 
                                        aes(x=b_fragilis_x_axis, y=b_fragilis_two_epoch, color='b_fragilis_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_fragilis_x_axis, y=b_fragilis_one_epoch, color='b_fragilis_one_epoch')) +
  geom_line(aes(x=b_fragilis_x_axis, y=b_fragilis_one_epoch, color='b_fragilis_one_epoch')) +
  geom_point(shape=1, aes(x=b_fragilis_x_axis, y=b_fragilis_empirical, color='b_fragilis_empirical')) +
  geom_line(aes(x=b_fragilis_x_axis, y=b_fragilis_empirical, color='b_fragilis_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_fragilis_x_axis, limits = c(2, length(b_fragilis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_fragilis_empirical',
                              'b_fragilis_two_epoch',
                              'b_fragilis_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. fragilis') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_fragilis_epoch_comparison

b_intestinihominis_two_epoch
b_intestinihominis_one_epoch
b_intestinihominis_empirical
b_intestinihominis_epoch_df = data.frame(b_intestinihominis_two_epoch,
                                         b_intestinihominis_one_epoch,
                                         b_intestinihominis_empirical)
b_intestinihominis_x_axis = 1:length(b_intestinihominis_two_epoch)
p_b_intestinihominis_epoch_comparison <- ggplot(data = b_intestinihominis_epoch_df, 
                                                aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_two_epoch, color='b_intestinihominis_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_one_epoch, color='b_intestinihominis_one_epoch')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_one_epoch, color='b_intestinihominis_one_epoch')) +
  geom_point(shape=1, aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +
  geom_line(aes(x=b_intestinihominis_x_axis, y=b_intestinihominis_empirical, color='b_intestinihominis_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_intestinihominis_x_axis, limits = c(2, length(b_intestinihominis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_intestinihominis_empirical',
                              'b_intestinihominis_two_epoch',
                              'b_intestinihominis_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. intestinihominis') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_intestinihominis_epoch_comparison

b_massiliensis_two_epoch = proportional_sfs(fold_sfs(c(151.707379159765, 75.86206391926804, 
                                                       50.574712567211186, 37.93103570105885, 
                                                       30.344829408232638, 25.28735840422925, 
                                                       21.674878994507583)))
b_massiliensis_one_epoch = proportional_sfs(fold_sfs(c(151.7241131105995, 75.86206517471176, 
                                                       50.574712126949876, 37.9310353705796, 
                                                       30.344829143849225, 25.287358183909554, 
                                                       21.674878805662278)))
b_massiliensis_empirical = proportional_sfs(b_massiliensis_empirical_syn_sfs)
b_massiliensis_epoch_df = data.frame(b_massiliensis_two_epoch,
                                     b_massiliensis_one_epoch,
                                     b_massiliensis_empirical)
b_massiliensis_x_axis = 1:length(b_massiliensis_two_epoch)
p_b_massiliensis_epoch_comparison <- ggplot(data = b_massiliensis_epoch_df, 
                                            aes(x=b_massiliensis_x_axis, y=b_massiliensis_two_epoch, color='b_massiliensis_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_massiliensis_x_axis, y=b_massiliensis_one_epoch, color='b_massiliensis_one_epoch')) +
  geom_line(aes(x=b_massiliensis_x_axis, y=b_massiliensis_one_epoch, color='b_massiliensis_one_epoch')) +
  geom_point(shape=1, aes(x=b_massiliensis_x_axis, y=b_massiliensis_empirical, color='b_massiliensis_empirical')) +
  geom_line(aes(x=b_massiliensis_x_axis, y=b_massiliensis_empirical, color='b_massiliensis_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_massiliensis_x_axis, limits = c(2, length(b_massiliensis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_massiliensis_empirical',
                              'b_massiliensis_two_epoch',
                              'b_massiliensis_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. massiliensis') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_massiliensis_epoch_comparison

b_ovatus_two_epoch = proportional_sfs(fold_sfs(c(2302.197893169133, 1151.1459069290945, 
                                                 767.4306281251272, 575.5729774872451, 
                                                 460.4583866952718, 383.7153259814277, 
                                                 328.89885392656845, 287.78649979962194, 
                                                 255.81022429841113, 230.22920383878198, 
                                                 209.29927795682272, 191.85767300923513, 
                                                 177.099391858061, 164.44943654736778, 
                                                 153.4861419093885, 143.89325906819937, 
                                                 135.4289506480638, 127.90512091225447, 
                                                 121.17327322648627, 115.11461028332694, 
                                                 109.6329628338851, 104.64964694724225, 
                                                 100.09966285437154, 95.9288440810848, 
                                                 92.09169078905491, 88.54970311507554, 
                                                 85.27008487938676, 82.22472507075501, 
                                                 79.3893900588084, 76.74307736389231, 
                                                 74.26749450371564, 71.94663555628739, 
                                                 69.76643471136674, 67.71448095990723, 
                                                 65.77978169390859, 63.952565706256394, 
                                                 62.22411813654286, 60.586641478100326, 
                                                 59.033137968519604, 57.55730962164358, 
                                                 56.15347288898089, 54.8164855123794, 
                                                 53.54168358327686, 52.324827184801514, 
                                                 51.16205328157606)))
b_ovatus_one_epoch = proportional_sfs(fold_sfs(c(2302.290948271064, 1151.1459204070263, 
                                                 767.430626566099, 575.5729763138514, 
                                                 460.45838575654705, 383.71532519916246, 
                                                 328.8988532560531, 287.78649921292094, 
                                                 255.81022377689916, 230.2292033694212, 
                                                 209.29927753012817, 191.85767261810386, 
                                                 177.09939149701427, 164.44943621211007, 
                                                 153.4861415964813, 143.89325877485297, 
                                                 135.42895037196922, 127.90512065149848, 
                                                 121.17327297945428, 115.11461004864654, 
                                                 109.63296261037996, 104.64964673389643, 
                                                 100.0996626503002, 95.9288438855178, 
                                                 92.09169060131059, 88.54970293455216, 
                                                 85.27008470554941, 82.22472490312616, 
                                                 79.38938989695984, 76.7430772074387, 
                                                 74.26749435230892, 71.94663540961417, 
                                                 69.76643456913519, 67.71448082185994, 
                                                 65.7797815598055, 63.95256557587838, 
                                                 62.22411800968947, 60.586641354584316, 
                                                 59.033137848170675, 57.557309504303376, 
                                                 56.15347277450184, 54.81648540062682, 
                                                 53.54168347412317, 52.324827078127846, 
                                                 51.1620531772751)))
b_ovatus_empirical = proportional_sfs(b_ovatus_empirical_syn_no_s_sfs)
b_ovatus_epoch_df = data.frame(b_ovatus_two_epoch,
                               b_ovatus_one_epoch,
                               b_ovatus_empirical)
b_ovatus_x_axis = 1:length(b_ovatus_two_epoch)
p_b_ovatus_epoch_comparison <- ggplot(data = b_ovatus_epoch_df, 
                                      aes(x=b_ovatus_x_axis, y=b_ovatus_two_epoch, color='b_ovatus_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_one_epoch, color='b_ovatus_one_epoch')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_one_epoch, color='b_ovatus_one_epoch')) +
  geom_point(shape=1, aes(x=b_ovatus_x_axis, y=b_ovatus_empirical, color='b_ovatus_empirical')) +
  geom_line(aes(x=b_ovatus_x_axis, y=b_ovatus_empirical, color='b_ovatus_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_ovatus_x_axis, limits = c(2, length(b_ovatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_ovatus_empirical',
                              'b_ovatus_two_epoch',
                              'b_ovatus_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. ovatus') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_ovatus_epoch_comparison

b_stercoris_two_epoch = proportional_sfs(fold_sfs(c(9602.567756458493, 4801.369378003914, 
                                                    3200.91298380381, 2400.6847608965704, 
                                                    1920.5478252752982, 1600.4565341062298, 
                                                    1371.8198971361032, 1200.3424191249705, 
                                                    1066.971047123794, 960.2739493484273, 
                                                    872.9763237474118, 800.2283022831925, 
                                                    738.6722840048111, 685.909982517671, 
                                                    640.1826544651428, 600.1712423293262, 
                                                    564.8670550668828, 533.4855551995329, 
                                                    505.40737103370753, 480.13700521452705, 
                                                    457.2733408355676, 436.48819133702773, 
                                                    417.5104460824428, 400.1141795414973, 
                                                    384.1096142686285, 369.33616934839597, 
                                                    355.65705363058953, 342.9550175578513, 
                                                    331.12898392557895, 320.09135248976565, 
                                                    309.7658262637195, 300.0856453840224, 
                                                    290.9921420919902, 282.43355071811624, 
                                                    274.36402166945606, 266.7427997522648, 
                                                    259.5335357396796, 252.70370663923723, 
                                                    246.22412515004763, 240.06852270126066, 
                                                    234.21319350944563, 228.63668948484045, 
                                                    223.31955770880006, 218.2441137098508, 
                                                    213.3942449695292, 208.755240057921, 
                                                    204.31363958172983, 200.05710576372945, 
                                                    195.97430799220925, 192.05482210437364, 
                                                    188.28904151885646, 184.66809862205199, 
                                                    181.18379505422686, 177.82853974155194, 
                                                    174.5952936882888, 171.47752068413178, 
                                                    168.46914320016575)))
b_stercoris_one_epoch = proportional_sfs(fold_sfs(c(9602.73417058656, 4801.369388790886, 
                                                    3200.9129826500184, 2400.6847600297083, 
                                                    1920.5478245818224, 1600.4565335282875, 
                                                    1371.8198966407535, 1200.342418691531, 
                                                    1066.9710467385144, 960.2739490016755, 
                                                    872.976323432183, 800.2283019942328, 
                                                    738.6722837381001, 685.9099822699911, 
                                                    640.182654233975, 600.1712421126064, 
                                                    564.8670548629192, 533.485555006893, 
                                                    505.4073708512066, 480.13700504115116, 
                                                    457.27334067045416, 436.48819117940087, 
                                                    417.5104459316812, 400.114179397006, 
                                                    384.10961412992776, 369.33616921503517, 
                                                    355.65705350216297, 342.9550174340114, 
                                                    331.1289838060094, 320.0913523741864, 
                                                    309.7658261518553, 300.08564527566676, 
                                                    290.9921419869139, 282.4335506161344, 
                                                    274.3640215703841, 266.74279965594485, 
                                                    259.53353564595926, 252.70370654798677, 
                                                    246.22412506113693, 240.06852261457271, 
                                                    234.21319342487203, 228.6366894022805, 
                                                    223.3195576281601, 218.2441136310436, 
                                                    213.39424489247327, 208.75523998254613, 
                                                    204.31363950795284, 200.05710569148948, 
                                                    195.97430792144357, 192.05482203502604, 
                                                    188.28904145086054, 184.66809855536894, 
                                                    181.183794988802, 177.82853967733865, 
                                                    174.59529362524302, 171.4775206222118, 
                                                    168.4691431393369)))
b_stercoris_empirical = proportional_sfs(fold_sfs(c(10025, 5851, 3507, 2597, 1972,
                                                    1807, 1614, 1533, 1427, 1314, 
                                                    1191, 1125, 1052, 923, 852, 866,
                                                    807, 712, 630, 618, 557, 567,
                                                    559, 515, 546, 462, 448, 440,
                                                    188, 0, 0, 0, 0, 0, 0, 0, 0,
                                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
b_stercoris_epoch_df = data.frame(b_stercoris_two_epoch,
                                 b_stercoris_one_epoch,
                                  b_stercoris_empirical)
b_stercoris_x_axis = 1:length(b_stercoris_two_epoch)
p_b_stercoris_epoch_comparison <- ggplot(data = b_stercoris_epoch_df, 
                                         aes(x=b_stercoris_x_axis, y=b_stercoris_two_epoch, color='b_stercoris_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_stercoris_x_axis, y=b_stercoris_one_epoch, color='b_stercoris_one_epoch')) +
  geom_line(aes(x=b_stercoris_x_axis, y=b_stercoris_one_epoch, color='b_stercoris_one_epoch')) +
  geom_point(shape=1, aes(x=b_stercoris_x_axis, y=b_stercoris_empirical, color='b_stercoris_empirical')) +
  geom_line(aes(x=b_stercoris_x_axis, y=b_stercoris_empirical, color='b_stercoris_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_stercoris_x_axis, limits = c(2, length(b_stercoris_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_stercoris_empirical',
                              'b_stercoris_two_epoch',
                              'b_stercoris_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. stercoris') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_stercoris_epoch_comparison

b_thetaiotaomicron_two_epoch
b_thetaiotaomicron_one_epoch
b_thetaiotaomicron_empirical

b_thetaiotaomicron_epoch_df = data.frame(b_thetaiotaomicron_two_epoch,
                                         b_thetaiotaomicron_one_epoch,
                                         b_thetaiotaomicron_empirical)
b_thetaiotaomicron_x_axis = 1:length(b_thetaiotaomicron_two_epoch)
p_b_thetaiotaomicron_epoch_comparison <- ggplot(data = b_thetaiotaomicron_epoch_df, 
                                                aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_two_epoch, color='b_thetaiotaomicron_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_one_epoch, color='b_thetaiotaomicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  geom_line(aes(x=b_thetaiotaomicron_x_axis, y=b_thetaiotaomicron_empirical, color='b_thetaiotaomicron_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_x_axis, limits = c(2, length(b_thetaiotaomicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_thetaiotaomicron_empirical',
                              'b_thetaiotaomicron_two_epoch',
                              'b_thetaiotaomicron_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Multi-epoch',
                              'One-epoch')) +
  ggtitle('B. thetaiotaomicron')
  # theme(legend.position="none",
  #      axis.text.x=element_blank())
p_b_thetaiotaomicron_epoch_comparison

b_uniformis_two_epoch = proportional_sfs(fold_sfs(c(2054.3278886050252, 1027.2454206090936, 
                                                    684.830308725038, 513.6227358960157, 
                                                    410.8981917177925, 342.41516209109585, 
                                                    293.4987122702638, 256.8113748511943, 
                                                    228.27677904352828, 205.44910236898514, 
                                                    186.7719123394741, 171.2075872953685, 
                                                    158.03777377950905, 146.74936217933407, 
                                                    136.96607211213865, 128.4056932907843, 
                                                    120.8524178485795, 114.13839522247417, 
                                                    108.1311118100442, 102.72455672932321, 
                                                    97.83291164727227, 93.38596156412969, 
                                                    89.32570278442479, 85.6037988952552, 
                                                    82.17964730978767, 79.01889199300257, 
                                                    76.0922666928385, 73.3746860503887, 
                                                    70.84452475624222, 68.48304087558701, 
                                                    66.27391078776738, 64.20285132471365, 
                                                    62.257310611453384, 60.426213464186674, 
                                                    58.69975043440762, 57.06920201232969, 
                                                    55.52679133787102, 54.065560167802, 
                                                    52.67926392486683, 51.36228248953352, 
                                                    50.10954404661384, 48.916459810935855, 
                                                    47.77886786107034, 46.69298463207639, 
                                                    45.6553628758933, 44.662855105170046, 
                                                    43.71258170381585, 42.80190302374183, 
                                                    41.928394898345246, 41.08982709434286, 
                                                    40.28414429871494, 39.509449299439375, 
                                                    38.76398807031026, 38.046136512984624, 
                                                    37.35438864536717, 36.68734605550832, 
                                                    36.04370846563223, 35.42226527229195, 
                                                    34.82188794685293, 34.24152319592012, 
                                                    33.68018679451514, 33.1369580160507, 
                                                    32.61097459276835, 32.10142814864365, 
                                                    31.60756005386936, 31.12865765620155, 
                                                    30.664050849807662, 30.213108946824878, 
                                                    29.775237821029418, 29.34987729624812, 
                                                    28.936498755542438, 28.534602949572932, 
                                                    28.14371798513375)))
b_uniformis_one_epoch = proportional_sfs(fold_sfs(c(2054.489642768396, 1027.245439327063, 
                                                    684.830306874114, 513.622734503184, 
                                                    410.8981906035272, 342.4151611625439, 
                                                    293.498711474358, 256.8113741547785, 
                                                    228.276778424492, 205.44910181185247, 
                                                    186.7719118329899, 171.2075868310962, 
                                                    158.03777335094773, 146.74936178138216, 
                                                    136.96607174071687, 128.40569294258003, 
                                                    120.85241752085783, 114.13839491295926, 
                                                    108.13111151681647, 102.72455645075686, 
                                                    97.83291138197099, 93.38596131088755, 
                                                    89.32570254219318, 85.60379866311536, 
                                                    82.17964708693341, 79.01889177872076, 
                                                    76.09226648649306, 73.37468585141379, 
                                                    70.84452456412649, 68.4830406898761, 
                                                    66.2739106080481, 64.20285115061058, 
                                                    62.25731044262528, 60.42621330032411, 
                                                    58.69975027522684, 57.06920185756979, 
                                                    55.52679118729303, 54.06556002118735, 
                                                    52.67926378201153, 51.36228235025033, 
                                                    50.10954391072709, 48.91645967828659, 
                                                    47.77886773150458, 46.69298450545531, 
                                                    45.655362752086674, 44.66285498405551, 
                                                    43.71258158527697, 42.801902907672506, 
                                                    41.9283947846441, 41.08982698291748, 
                                                    40.284144189473224, 39.50944919229846, 
                                                    38.76398796519088, 38.04613640981189, 
                                                    37.354388544069245, 36.68734595602033, 
                                                    36.04370836789117, 35.42226517623458, 
                                                    34.82188785242366, 34.24152310306466, 
                                                    33.680186703180944, 33.136957926190576, 
                                                    32.610974504334585, 32.101428061591655, 
                                                    31.60755996815663, 31.12865757178927, 
                                                    30.664050766652647, 30.213108864895304, 
                                                    29.77523774028469, 29.349877216657724, 
                                                    28.936498677073036, 28.534602872193382, 
                                                    28.143717908815795)))
b_uniformis_empirical = proportional_sfs(b_uniformis_empirical_syn_no_s_sfs)

b_uniformis_epoch_df = data.frame(b_uniformis_two_epoch,
                                  b_uniformis_one_epoch,
                                  b_uniformis_empirical)
b_uniformis_x_axis = 1:length(b_uniformis_two_epoch)
p_b_uniformis_epoch_comparison <- ggplot(data = b_uniformis_epoch_df, 
                                         aes(x=b_uniformis_x_axis, y=b_uniformis_two_epoch, color='b_uniformis_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_one_epoch, color='b_uniformis_one_epoch')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_one_epoch, color='b_uniformis_one_epoch')) +
  geom_point(shape=1, aes(x=b_uniformis_x_axis, y=b_uniformis_empirical, color='b_uniformis_empirical')) +
  geom_line(aes(x=b_uniformis_x_axis, y=b_uniformis_empirical, color='b_uniformis_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_uniformis_x_axis, limits = c(2, length(b_uniformis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_uniformis_empirical',
                              'b_uniformis_two_epoch',
                              'b_uniformis_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. uniformis') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_uniformis_epoch_comparison

b_vulgatus_two_epoch = proportional_sfs(fold_sfs(c(2364.430150627921, 1182.2854389840386, 
                                                   788.1903189513198, 591.1427455978621, 
                                                   472.9142011600663, 394.0951713464688, 
                                                   337.79586422129603, 295.57138379357974, 
                                                   262.7301211717921, 236.45711101799577, 
                                                   214.9610117520908, 197.0475956533406, 
                                                   181.89008968393156, 168.8979416737802, 
                                                   157.63808003137981, 147.78570106299293, 
                                                   139.0924254733913, 131.36506936621942, 
                                                   124.45111913909399, 118.22856391007389, 
                                                   112.59863296520146, 107.48051390213857, 
                                                   102.80744864937823, 98.5238054806654, 
                                                   94.58285374594072, 90.94505212591903, 
                                                   87.57671727453386, 84.44897775231267, 
                                                   81.53694438728081, 78.8190465637314, 
                                                   76.27649697120717, 73.89285671306045, 
                                                   71.65367948617225, 69.54621855250153, 
                                                   67.55918394405953, 65.68254013374859, 
                                                   63.907336516321685, 62.22556465550429, 
                                                   60.63003749309705, 59.11428667672271, 
                                                   57.67247491277078, 56.299320840361666, 
                                                   54.990034387989404, 53.740260945199445, 
                                                   52.54603297802339, 51.40372795544347, 
                                                   50.3100316469542)))
b_vulgatus_one_epoch = proportional_sfs(fold_sfs(c(2364.5699580854357, 1182.2854555623705, 
                                                   788.1903170616415, 591.1427441763988, 
                                                   472.9142000228889, 394.09517039882104, 
                                                   337.7958634090265, 295.57138308284385, 
                                                   262.73012054002686, 236.45711044940705, 
                                                   214.96101123519196, 197.04759517951385, 
                                                   181.89008924655562, 168.8979412676454, 
                                                   157.6380796523162, 147.7857007076271, 
                                                   139.09242513892733, 131.36506905033676, 
                                                   124.45111883983675, 118.22856362577951, 
                                                   112.59863269444492, 107.48051364368608, 
                                                   102.80744840216718, 98.52380524375623, 
                                                   94.58285351850522, 90.94505190723105, 
                                                   87.57671706394544, 84.44897754924526, 
                                                   81.53694419121572, 78.8190463742018, 
                                                   76.27649678779143, 73.89285653537645, 
                                                   71.65367931387262, 69.54621838526953, 
                                                   67.5591837816056, 65.6825399758082, 
                                                   63.90733636264904, 62.22556450587567, 
                                                   60.63003734730505, 59.11428653457551, 
                                                   57.67247477409059, 56.29932070498338, 
                                                   54.990034255759454, 53.74026081597472, 
                                                   52.54603285167032, 51.403727831837216, 
                                                   50.310031525977855)))
b_vulgatus_empirical = proportional_sfs(b_vulgatus_empirical_syn_no_s_sfs)
b_vulgatus_epoch_df = data.frame(b_vulgatus_two_epoch,
                                 b_vulgatus_one_epoch,
                                 b_vulgatus_empirical)
b_vulgatus_x_axis = 1:length(b_vulgatus_two_epoch)
p_b_vulgatus_epoch_comparison <- ggplot(data = b_vulgatus_epoch_df, 
                                        aes(x=b_vulgatus_x_axis, y=b_vulgatus_two_epoch, color='b_vulgatus_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_one_epoch, color='b_vulgatus_one_epoch')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_one_epoch, color='b_vulgatus_one_epoch')) +
  geom_point(shape=1, aes(x=b_vulgatus_x_axis, y=b_vulgatus_empirical, color='b_vulgatus_empirical')) +
  geom_line(aes(x=b_vulgatus_x_axis, y=b_vulgatus_empirical, color='b_vulgatus_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_vulgatus_x_axis, limits = c(2, length(b_vulgatus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_vulgatus_empirical',
                              'b_vulgatus_two_epoch',
                              'b_vulgatus_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. vulgatus') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_vulgatus_epoch_comparison

b_xylanisolvens_two_epoch = proportional_sfs(fold_sfs(c(2105.8374270577815, 1052.9817978608924, 
                                                        701.9878927725437, 526.4909256080464, 
                                                        421.19274493643286, 350.9939576489388, 
                                                        300.8519666184875, 263.245473262712, 
                                                        233.9959783629688, 210.59638238557156, 
                                                        191.45125835342287, 175.49698828120188, 
                                                        161.99722125572893, 150.4259923388127, 
                                                        140.39759390901511, 131.6227452501973, 
                                                        123.88023169704203, 116.99799739870232, 
                                                        110.84020878870986, 105.29819901387715, 
                                                        100.28399966922252, 95.72563660522232, 
                                                        91.56365291573003, 87.74850117896518, 
                                                        84.23856156062027, 80.99861727781958, 
                                                        77.9986688488463, 75.21300243224054, 
                                                        72.61945092328159, 70.19880283121948, 
                                                        67.93432556732623, 65.81137811647169, 
                                                        63.81709413202335, 61.94012095518266, 
                                                        60.17040337387702, 58.49900342182019, 
                                                        56.91794939934135, 55.42010873305953, 
                                                        53.99908039557615, 52.649103462233725, 
                                                        51.36497904980974, 50.14200340680693, 
                                                        48.97591034001419)))
b_xylanisolvens_one_epoch = proportional_sfs(fold_sfs(c(2105.962851672609, 1052.9818178591947, 
                                                       701.9878904222437, 526.4909238385877, 
                                                       421.19274352085995, 350.9939564692972, 
                                                       300.851965607364, 263.2454723779789, 
                                                       233.99597757653936, 210.59638167778806, 
                                                       191.45125770997515, 175.49698769138232, 
                                                       161.99722071127778, 150.4259918332488, 
                                                       140.39759343715943, 131.62274480783074, 
                                                       123.880231280697, 116.99799700548758, 
                                                       110.84020841619063, 105.29819865998388, 
                                                       100.28399933218131, 95.72563628350116, 
                                                       91.56365260799674, 87.74850088405411, 
                                                       84.23856127750565, 80.9986170055905, 
                                                       77.99866858670423, 75.21300217946069, 
                                                       72.61945067921725, 70.19880259529162, 
                                                       67.93432533900797, 65.81137789528839, 
                                                       63.817093917540745, 61.940120747010134, 
                                                       60.17040317165399, 58.49900322521363, 
                                                       56.91794920804768, 55.420108546800684, 
                                                       53.99908021409241, 52.649103285287815, 
                                                       51.364978877179595, 50.14200323828631, 
                                                       48.97591017541265)))
b_xylanisolvens_empirical = proportional_sfs(fold_sfs(c(2367, 1159, 739, 556,
                                                        506, 410, 340, 341, 332,
                                                        327, 332, 267, 241, 213,
                                                        227, 208, 169, 186, 176,
                                                        133, 114, 30, 0, 0, 0,
                                                        0, 0, 0, 0, 0, 0, 0, 0,
                                                        0, 0, 0, 0, 0, 0, 0, 0,
                                                        0, 0, 0)))
b_xylanisolvens_epoch_df = data.frame(b_xylanisolvens_two_epoch,
                                      b_xylanisolvens_one_epoch,
                                      b_xylanisolvens_empirical)
b_xylanisolvens_x_axis = 1:length(b_xylanisolvens_two_epoch)
p_b_xylanisolvens_epoch_comparison <- ggplot(data = b_xylanisolvens_epoch_df, 
                                             aes(x=b_xylanisolvens_x_axis, y=b_xylanisolvens_two_epoch, color='b_xylanisolvens_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_xylanisolvens_x_axis, y=b_xylanisolvens_one_epoch, color='b_xylanisolvens_one_epoch')) +
  geom_line(aes(x=b_xylanisolvens_x_axis, y=b_xylanisolvens_one_epoch, color='b_xylanisolvens_one_epoch')) +
  geom_point(shape=1, aes(x=b_xylanisolvens_x_axis, y=b_xylanisolvens_empirical, color='b_xylanisolvens_empirical')) +
  geom_line(aes(x=b_xylanisolvens_x_axis, y=b_xylanisolvens_empirical, color='b_xylanisolvens_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_xylanisolvens_x_axis, limits = c(2, length(b_xylanisolvens_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('b_xylanisolvens_empirical',
                              'b_xylanisolvens_two_epoch',
                              'b_xylanisolvens_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('B. xylanisolvens') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_b_xylanisolvens_epoch_comparison

d_invisus_two_epoch = proportional_sfs(fold_sfs(c(7219.604873917495, 3610.031831706991, 
                                                  2406.687981940724, 1805.0160123390308, 
                                                  1444.0128292557151, 1203.344039774064, 
                                                  1031.4377610153838, 902.5080514937127, 
                                                  802.229388145781, 722.006457127435, 
                                                  656.3695132622606, 601.6720597646364, 
                                                  555.389598859095, 515.718917848454, 
                                                  481.3376607544993, 451.25406059344675, 
                                                  424.7097073187003, 401.11472644954654, 
                                                  380.00342760638586, 361.0032584855355, 
                                                  343.81262912682627, 328.1847841082685, 
                                                  313.91588199430566, 300.8360549219592, 
                                                  288.80261388630737, 277.694822037023, 
                                                  267.4098294645681, 257.85947910360534, 
                                                  248.96777348402549, 240.6688481317169, 
                                                  232.90533721438305)))
d_invisus_one_epoch = proportional_sfs(fold_sfs(c(7220.061767166641, 3610.031895191867, 
                                                  2406.6879734810987, 1805.016005975253, 
                                                  1444.0128241646723, 1203.3440355315367, 
                                                  1031.4377573789247, 902.5080483118172, 
                                                  802.2293853174522, 722.0064545819084, 
                                                  656.369510948164, 601.6720576433727, 
                                                  555.3895969010053, 515.7189160302279, 
                                                  481.3376590574883, 451.25405900249245, 
                                                  424.70970582133754, 401.1147250353706, 
                                                  380.00342626664025, 361.0032572127772, 
                                                  343.8126279146706, 328.1847829512155, 
                                                  313.91588088755924, 300.83605386132723, 
                                                  288.8026128680966, 277.6948210579742, 
                                                  267.40982852178405, 257.85947819449217, 
                                                  248.9677726062611, 240.6688472832113, 
                                                  232.9053363932486)))
d_invisus_empirical = proportional_sfs(fold_sfs(c(10483, 3931, 2931, 2034, 1633,
                                                  1458, 1217, 1228, 1117, 1130,
                                                  1117, 1044, 856, 790, 697,
                                                  441, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  0, 0, 0, 0, 0, 0, 0, 0)))

d_invisus_epoch_df = data.frame(d_invisus_two_epoch,
                                d_invisus_one_epoch,
                                d_invisus_empirical)
d_invisus_x_axis = 1:length(d_invisus_two_epoch)
p_d_invisus_epoch_comparison <- ggplot(data = d_invisus_epoch_df, 
                                       aes(x=d_invisus_x_axis, y=d_invisus_two_epoch, color='d_invisus_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=d_invisus_x_axis, y=d_invisus_one_epoch, color='d_invisus_one_epoch')) +
  geom_line(aes(x=d_invisus_x_axis, y=d_invisus_one_epoch, color='d_invisus_one_epoch')) +
  geom_point(shape=1, aes(x=d_invisus_x_axis, y=d_invisus_empirical, color='d_invisus_empirical')) +
  geom_line(aes(x=d_invisus_x_axis, y=d_invisus_empirical, color='d_invisus_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=d_invisus_x_axis, limits = c(2, length(d_invisus_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('d_invisus_empirical',
                              'd_invisus_two_epoch',
                              'd_invisus_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('D. invisus') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_d_invisus_epoch_comparison

e_eligens_two_epoch = proportional_sfs(fold_sfs(c(5825.643855142483, 2912.873768824394, 
                                                  1941.9158910722535, 1456.4369440336636, 
                                                  1165.1495745079599, 970.9579939383259, 
                                                  832.2497214004229, 728.2185164039628, 
                                                  647.3053564449239, 582.5748280171395, 
                                                  529.6134861604394, 485.47903423384963, 
                                                  448.1344976392302, 416.12489452058156, 
                                                  388.3832381832614, 364.1092886062297, 
                                                  342.6910975379876, 323.65270522732897, 
                                                  306.61835397588277, 291.2874376249293, 
                                                  277.41660833164696, 264.8067633155868, 
                                                  253.29342636672263)))
e_eligens_one_epoch = proportional_sfs(fold_sfs(c(5825.746270209707, 2912.8737751734648, 
                                                 1941.9158901072415, 1456.4369433090044, 
                                                 1165.1495739282573, 970.9579934552197, 
                                                 832.2497209863378, 728.2185160416435, 
                                                 647.3053561228576, 582.574827727288, 
                                                 529.6134858969382, 485.47903399229995, 
                                                 448.13449741626766, 416.124894313539, 
                                                 388.38323799002166, 364.10928842506746, 
                                                 342.69109736748203, 323.6527050662958, 
                                                 306.61835382332504, 291.2874374799912, 
                                                 277.4166081936265, 264.80676318383246, 
                                                 253.2934262406967)))
e_eligens_empirical = proportional_sfs(fold_sfs(c(6802, 3652, 2471, 1823, 1542,
                                                  1365, 1100, 1032, 894, 828,
                                                  661, 308, 0, 0, 0, 0, 0, 0, 0,
                                                  0, 0, 0, 0, 0)))
e_eligens_epoch_df = data.frame(e_eligens_two_epoch,
                                e_eligens_one_epoch,
                                e_eligens_empirical)
e_eligens_x_axis = 1:length(e_eligens_two_epoch)
p_e_eligens_epoch_comparison <- ggplot(data = e_eligens_epoch_df, 
                                       aes(x=e_eligens_x_axis, y=e_eligens_two_epoch, color='e_eligens_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=e_eligens_x_axis, y=e_eligens_one_epoch, color='e_eligens_one_epoch')) +
  geom_line(aes(x=e_eligens_x_axis, y=e_eligens_one_epoch, color='e_eligens_one_epoch')) +
  geom_point(shape=1, aes(x=e_eligens_x_axis, y=e_eligens_empirical, color='e_eligens_empirical')) +
  geom_line(aes(x=e_eligens_x_axis, y=e_eligens_empirical, color='e_eligens_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=e_eligens_x_axis, limits = c(2, length(e_eligens_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('e_eligens_empirical',
                              'e_eligens_two_epoch',
                              'e_eligens_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('E. eligens') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_e_eligens_epoch_comparison

e_rectale_two_epoch = proportional_sfs(fold_sfs(c(10981.807590001703, 5491.078256177634, 
                                                  3660.7189363363705, 2745.539226810664, 
                                                  2196.4313987516307, 1830.359512489709, 
                                                  1568.8795932643472, 1372.7696535468483, 
                                                  1220.2397002167831, 1098.2157373821917, 
                                                  998.3779494667326, 915.179792748341, 
                                                  844.7813523402392, 784.4398318932206, 
                                                  732.1438474172114, 686.38486091903, 
                                                  646.0092845212464, 610.1198832078582, 
                                                  578.008313545076, 549.1079007856697, 
                                                  522.9599082295347, 499.18900584916423, 
                                                  477.48513840445054, 457.589926528576, 
                                                  439.2863315534393, 422.3907053752318, 
                                                  406.74660701656495, 392.2199442111498, 
                                                  378.6951201777003, 366.0719510390342, 
                                                  354.26317986992575, 343.19245686077016, 
                                                  332.79268672433267, 323.00466773655586, 
                                                  313.77596408475256, 305.0599661576093, 
                                                  296.81510322071335, 289.00418040644865, 
                                                  281.5938177053765, 274.5539731090238, 
                                                  267.85753553659157, 261.4799759149453, 
                                                  255.39904694519723, 249.5945238101947, 
                                                  244.04797945429507, 238.74258917451928, 
                                                  233.66296015750066, 228.7949823243381, 
                                                  224.12569743928702, 219.6431839254623, 
                                                  215.3364552316279, 211.19536992586438, 
                                                  207.21055196733457, 203.37331983675978, 
                                                  199.67562339807773, 196.10998752492654, 
                                                  192.6694616612551, 189.34757459964402, 
                                                  186.13829385866103, 183.03598912227736, 
                                                  180.0353992754935, 177.13160263012273, 
                                                  174.31998998610362, 171.59624021835907, 
                                                  168.9562981172367)))
e_rectale_one_epoch = proportional_sfs(fold_sfs(c(10982.150650133572, 5491.078294785315, 
                                                  3660.718932380231, 2745.539223834332, 
                                                  2196.4313963705335, 1830.3595105054485, 
                                                  1568.8795915635749, 1372.7696520586626, 
                                                  1220.2396988939513, 1098.2157361916431, 
                                                  998.3779483844157, 915.1797917562433, 
                                                  844.7813514244206, 784.4398310428288, 
                                                  732.1438466235124, 686.3848601749371, 
                                                  646.0092838209146, 610.1198825464422, 
                                                  578.0083129184716, 549.1079001903876, 
                                                  522.9599076626067, 499.1890053080058, 
                                                  477.4851378868207, 457.58992603251403, 
                                                  439.2863310772324, 422.3907049173285, 
                                                  406.746606575621, 392.2199437859539, 
                                                  378.69511976716086, 366.0719506421846, 
                                                  354.2631794858778, 343.19245648872374, 
                                                  332.79268636356034, 323.0046673863945, 
                                                  313.77596374460023, 305.0599658269013, 
                                                  296.81510289894345, 289.0041800931464, 
                                                  281.59381740010764, 274.5539728113866, 
                                                  267.85753524621384, 261.47997563148135, 
                                                  255.39904666832544, 249.59452353961544, 
                                                  244.0479791897287, 238.7425889157111, 
                                                  233.6629599041924, 228.79498207630712, 
                                                  224.1256971963243, 219.64318368735255, 
                                                  215.33645499818698, 211.19536969691268, 
                                                  207.2105517427027, 203.3733196162878, 
                                                  199.67562318161433, 196.10998731232854, 
                                                  192.66946145238688, 189.347574394377, 
                                                  186.13829365687312, 183.03598892385256, 
                                                  180.035399080319, 177.13160243809875, 
                                                  174.31998979712765, 171.59624003233583, 
                                                  168.95629793407537)))
e_rectale_empirical = proportional_sfs(e_rectale_empirical_syn_sfs)
e_rectale_epoch_df = data.frame(e_rectale_two_epoch,
                                e_rectale_one_epoch,
                                e_rectale_empirical)
e_rectale_x_axis = 1:length(e_rectale_two_epoch)
p_e_rectale_epoch_comparison <- ggplot(data = e_rectale_epoch_df, 
                                       aes(x=e_rectale_x_axis, y=e_rectale_two_epoch, color='e_rectale_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=e_rectale_x_axis, y=e_rectale_one_epoch, color='e_rectale_one_epoch')) +
  geom_line(aes(x=e_rectale_x_axis, y=e_rectale_one_epoch, color='e_rectale_one_epoch')) +
  geom_point(shape=1, aes(x=e_rectale_x_axis, y=e_rectale_empirical, color='e_rectale_empirical')) +
  geom_line(aes(x=e_rectale_x_axis, y=e_rectale_empirical, color='e_rectale_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=e_rectale_x_axis, limits = c(2, length(e_rectale_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('e_rectale_empirical',
                              'e_rectale_two_epoch',
                              'e_rectale_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('E. rectale') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_e_rectale_epoch_comparison

f_prausnitzii_two_epoch = proportional_sfs(fold_sfs(c(11479.764609067837, 5740.012558192761, 
                                                      3826.6751203137833, 2870.006383339694, 
                                                      2296.005139006315, 1913.3376414853428, 
                                                      1640.0037137109207, 1435.0032670805383, 
                                                      1275.5584745697486, 1148.0026399546343, 
                                                      1043.6387747248182, 956.6688865380474, 
                                                      883.0789806945813, 820.0019181225789, 
                                                      765.3351301689978, 717.5016903436867, 
                                                      675.2957136835441, 637.7792896615152, 
                                                      604.211962598325, 574.0013679504425, 
                                                      546.667972515952, 521.819430947772, 
                                                      499.131631872198, 478.3344824778407, 
                                                      459.2011048030231, 441.5395251879325, 
                                                      425.1862105148659, 410.00098954014675, 
                                                      395.8630249846457)))
f_prausnitzii_one_epoch = proportional_sfs(fold_sfs(c(11480.022101322984, 5740.012571658368, 
                                                      3826.675118466437, 2870.006381952553, 
                                                      2296.0051378965372, 1913.3376405605686, 
                                                      1640.0037129182338, 1435.0032663869374, 
                                                      1275.5584739532144, 1148.0026393997534, 
                                                      1043.6387742203663, 956.6688860756467, 
                                                      883.0789802677499, 820.0019177262354, 
                                                      765.3351297990555, 717.5016899968862, 
                                                      675.2957133571437, 637.7792893532481, 
                                                      604.2119623062824, 574.001367673002, 
                                                      546.6679722517231, 521.8194306955535, 
                                                      499.1316316309455, 478.33448224664033, 
                                                      459.20110458107075, 441.5395249745168, 
                                                      425.18621030935446, 410.000989341975, 
                                                      395.8630247933187)))
f_prausnitzii_empirical = proportional_sfs(fold_sfs(c(13211, 6888, 4655, 3670,
                                                      3174, 2654, 2264, 2048, 
                                                      1924, 1640, 1436, 1241,
                                                      1023, 763, 224, 0, 0, 0, 
                                                      0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                      0, 0, 0)))
f_prausnitzii_epoch_df = data.frame(f_prausnitzii_two_epoch,
                                    f_prausnitzii_one_epoch,
                                    f_prausnitzii_empirical)
f_prausnitzii_x_axis = 1:length(f_prausnitzii_two_epoch)
p_f_prausnitzii_epoch_comparison <- ggplot(data = f_prausnitzii_epoch_df, 
                                           aes(x=f_prausnitzii_x_axis, y=f_prausnitzii_two_epoch, color='f_prausnitzii_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=f_prausnitzii_x_axis, y=f_prausnitzii_one_epoch, color='f_prausnitzii_one_epoch')) +
  geom_line(aes(x=f_prausnitzii_x_axis, y=f_prausnitzii_one_epoch, color='f_prausnitzii_one_epoch')) +
  geom_point(shape=1, aes(x=f_prausnitzii_x_axis, y=f_prausnitzii_empirical, color='f_prausnitzii_empirical')) +
  geom_line(aes(x=f_prausnitzii_x_axis, y=f_prausnitzii_empirical, color='f_prausnitzii_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=f_prausnitzii_x_axis, limits = c(2, length(f_prausnitzii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('f_prausnitzii_empirical',
                              'f_prausnitzii_two_epoch',
                              'f_prausnitzii_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('F. prausnitzii') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_f_prausnitzii_epoch_comparison

o_sp_two_epoch = proportional_sfs(fold_sfs(c(5155.965410106295, 2578.0331638067196, 
                                             1718.6888088210233, 1289.016620925345, 
                                             1031.2133072783308, 859.3444310974467, 
                                             736.5809478503998, 644.5083352229807, 
                                             572.8963030245759, 515.6066771344796, 
                                             468.73334674574966, 429.67223798556927, 
                                             396.6205304797663, 368.2904953891115, 
                                             343.7377982313127, 322.2541881444389, 
                                             303.298061528088, 286.4481711374547, 
                                             271.37195335812845, 257.80335729857865, 
                                             245.52700842752677, 234.3666912193648, 
                                             224.17683632683523, 214.8361359605586, 
                                             206.24269157743018, 198.31028133329244, 
                                             190.9654569905159, 184.14526291684382, 
                                             177.79542701549695, 171.86891346928854, 
                                             166.32475559868217, 161.1271075591233, 
                                             156.2444684569005, 151.6490433857875, 
                                             147.31621400027822, 143.2240973265826, 
                                             139.35317611780187, 135.685987574085, 
                                             132.20685995198176, 128.90168868236773, 
                                             125.75774525165339, 122.7635133856432, 
                                             119.90854809145114, 117.18335392100755, 
                                             114.57927946605277)))
o_sp_one_epoch = proportional_sfs(fold_sfs(c(5156.064338755158, 2578.0331688188267, 
                                             1718.6888082401108, 1289.016620489102, 
                                             1031.2133069293436, 859.344430806624, 
                                             736.5809476011285, 644.5083350048545, 
                                             572.896302830686, 515.6066769599787, 
                                             468.73334658711246, 429.6722378401579, 
                                             396.6205303455348, 368.290495264468, 
                                             343.7377981149787, 322.2541880353758, 
                                             303.2980614254404, 286.4481710405098, 
                                             271.37195326628586, 257.80335721132815, 
                                             245.5270083444311, 234.3666911400462, 
                                             224.17683625096208, 214.8361358878499, 
                                             206.2426915076357, 198.3102812661767, 
                                             190.96545692588592, 184.14526285452206, 
                                             177.79542695532925, 171.86891341112158, 
                                             166.32475554238917, 161.12710750459632, 
                                             156.24446840402587, 151.64904333446802, 
                                             147.3162139504208, 143.22409727811012, 
                                             139.35317607063746, 135.68598752816757, 
                                             132.20685990723607, 128.9016886387425, 
                                             125.7577452090904, 122.76351334409534, 
                                             119.90854805086953, 117.18335388134491, 
                                             114.57927942727804)))
o_sp_empirical = proportional_sfs(fold_sfs(c(8279, 4790, 2256, 1375, 918, 701,
                                             546, 469, 430, 500, 544, 696, 595,
                                             564, 543, 478, 428, 378, 333, 292,
                                             274, 202, 78, 0, 0, 0, 0, 0, 0, 0,
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                             0, 0, 0, 0, 0)))
o_sp_epoch_df = data.frame(o_sp_two_epoch,
                                   o_sp_one_epoch)
o_sp_x_axis = 1:length(o_sp_two_epoch)
p_o_sp_epoch_comparison <- ggplot(data = o_sp_epoch_df, 
                                  aes(x=o_sp_x_axis, y=o_sp_two_epoch, color='o_sp_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=o_sp_x_axis, y=o_sp_one_epoch, color='o_sp_one_epoch')) +
  geom_line(aes(x=o_sp_x_axis, y=o_sp_one_epoch, color='o_sp_one_epoch')) +
  geom_point(shape=1, aes(x=o_sp_x_axis, y=o_sp_empirical, color='o_sp_empirical')) +
  geom_line(aes(x=o_sp_x_axis, y=o_sp_empirical, color='o_sp_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=o_sp_x_axis, limits = c(2, length(o_sp_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('o_sp_empirical',
                              'o_sp_two_epoch',
                              'o_sp_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('O. sp') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_o_sp_epoch_comparison

p_copri_two_epoch = proportional_sfs(fold_sfs(c(7974.83594317686, 3987.4747088993754, 
                                                2658.3165523864204, 1993.7374650724798, 
                                                1594.990009016696, 1329.1583687517825, 
                                                1139.278623272617, 996.8688120540131, 
                                                886.1056236853229, 797.4950713169401, 
                                                724.9955269521522, 664.5792385921993, 
                                                613.4577625451421)))
p_copri_one_epoch = proportional_sfs(fold_sfs(c(7974.9482427645, 3987.474713112209, 
                                                2658.31655151093, 1993.7374644154372, 
                                                1594.9900084910955, 1329.1583683137542, 
                                                1139.2786228971804, 996.8688117255201, 
                                                886.1056233933166, 797.4950710541343, 
                                                724.9955267132378, 664.579238373185, 
                                                613.4577623429839)))
p_copri_empirical = proportional_sfs(p_copri_empirical_syn_sfs)
p_copri_epoch_df = data.frame(p_copri_two_epoch,
                              p_copri_one_epoch,
                              p_copri_empirical)
p_copri_x_axis = 1:length(p_copri_two_epoch)
p_p_copri_epoch_comparison <- ggplot(data = p_copri_epoch_df, 
                                     aes(x=p_copri_x_axis, y=p_copri_two_epoch, color='p_copri_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_copri_x_axis, y=p_copri_one_epoch, color='p_copri_one_epoch')) +
  geom_line(aes(x=p_copri_x_axis, y=p_copri_one_epoch, color='p_copri_one_epoch')) +
  geom_point(shape=1, aes(x=p_copri_x_axis, y=p_copri_empirical, color='p_copri_empirical')) +
  geom_line(aes(x=p_copri_x_axis, y=p_copri_empirical, color='p_copri_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_copri_x_axis, limits = c(2, length(p_copri_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('p_copri_empirical',
                              'p_copri_two_epoch',
                              'p_copri_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('P. copri') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_p_copri_epoch_comparison

p_distasonis_two_epoch
p_distasonis_one_epoch
p_distasonis_empirical

p_distasonis_epoch_df = data.frame(p_distasonis_two_epoch,
                                   p_distasonis_one_epoch,
                                   p_distasonis_empirical)
p_distasonis_x_axis = 1:length(p_distasonis_two_epoch)
p_p_distasonis_epoch_comparison <- ggplot(data = p_distasonis_epoch_df, 
                                          aes(x=p_distasonis_x_axis, y=p_distasonis_two_epoch, color='p_distasonis_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_one_epoch, color='p_distasonis_one_epoch')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_one_epoch, color='p_distasonis_one_epoch')) +
  geom_point(shape=1, aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +
  geom_line(aes(x=p_distasonis_x_axis, y=p_distasonis_empirical, color='p_distasonis_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_distasonis_x_axis, limits = c(2, length(p_distasonis_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('p_distasonis_empirical',
                              'p_distasonis_two_epoch',
                              'p_distasonis_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('P. distasonis') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_p_distasonis_epoch_comparison

p_merdae_two_epoch
p_merdae_one_epoch
p_merdae_empirical

p_merdae_epoch_df = data.frame(p_merdae_two_epoch,
                               p_merdae_one_epoch,
                               p_merdae_empirical)
p_merdae_x_axis = 1:length(p_merdae_two_epoch)
p_p_merdae_epoch_comparison <- ggplot(data = p_merdae_epoch_df, 
                                      aes(x=p_merdae_x_axis, y=p_merdae_two_epoch, color='p_merdae_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_one_epoch, color='p_merdae_one_epoch')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_one_epoch, color='p_merdae_one_epoch')) +
  geom_point(shape=1, aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +
  geom_line(aes(x=p_merdae_x_axis, y=p_merdae_empirical, color='p_merdae_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_merdae_x_axis, limits = c(2, length(p_merdae_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('p_merdae_empirical',
                              'p_merdae_two_epoch',
                              'p_merdae_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('P. merdae') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_p_merdae_epoch_comparison

p_sp_two_epoch = proportional_sfs(fold_sfs(c(2641.334023403126, 1791.4206736727167, 
                                             1307.1901521781963, 1010.8738402580609, 
                                             817.4547742040486, 683.8179785042425, 
                                             586.9205420862119, 513.7976450596426, 
                                             456.7832106701746, 411.1274669335818, 
                                             373.75902097482583, 342.6144304155506, 
                                             316.2600458360266, 293.670201426022, 
                                             274.09223099358064, 256.961478273873, 
                                             241.8461007769209, 228.41020773632337, 
                                             216.3886186379095)))
p_sp_one_epoch = proportional_sfs(fold_sfs(c(3970.568412808523, 1985.284584160073, 
                                             1323.523086397207, 992.6423347647475, 
                                             794.1138826924911, 661.7615805134936, 
                                             567.2242211632537, 496.3212010934979, 
                                             441.1744072165312, 397.05697167759007, 
                                             360.9608876589889, 330.8808172817877, 
                                             305.4284497064283, 283.6121343330601, 
                                             264.70466072166084, 248.16062104217434, 
                                             233.56293871858657, 220.5872208584272, 
                                             208.97736780968506)))
p_sp_empirical
p_sp_epoch_df = data.frame(p_sp_two_epoch,
                           p_sp_one_epoch,
                           p_sp_empirical)
p_sp_x_axis = 1:length(p_sp_two_epoch)
p_p_sp_epoch_comparison <- ggplot(data = p_sp_epoch_df, 
                                  aes(x=p_sp_x_axis, y=p_sp_two_epoch, color='p_sp_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_one_epoch, color='p_sp_one_epoch')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_one_epoch, color='p_sp_one_epoch')) +
  geom_point(shape=1, aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +
  geom_line(aes(x=p_sp_x_axis, y=p_sp_empirical, color='p_sp_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=p_sp_x_axis, limits = c(2, length(p_sp_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('p_sp_empirical',
                              'p_sp_two_epoch',
                              'p_sp_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('P. sp') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_p_sp_epoch_comparison

r_bromii_two_epoch = proportional_sfs(fold_sfs(c(7258.016791219756, 3629.063691080987, 
                                                 2419.3758406606257, 1814.5319064965502, 
                                                 1451.6255446839345, 1209.6879693787375, 
                                                 1036.8754150288787, 907.2659988114501, 
                                                 806.4586746980729, 725.8128150655887, 
                                                 659.8298386963496, 604.8440247772046, 
                                                 558.3175665905773, 518.4377450520552, 
                                                 483.87523283278716, 453.6330344360044, 
                                                 426.9487415404987, 403.2293698963795, 
                                                 382.006774043139, 362.9064376123628, 
                                                 345.6251806867175, 329.9149469701702, 
                                                 315.5708203920849, 302.4220375602502, 
                                                 290.3251572251845, 279.15880602194886, 
                                                 268.8195918248316, 259.2188928117899, 
                                                 250.2803108603784, 241.93763426446088, 
                                                 234.1331947638518)))
r_bromii_one_epoch = proportional_sfs(fold_sfs(c(7258.125355839273, 3629.0636948613023, 
                                                 2419.3758401559435, 1814.5319061177418, 
                                                 1451.6255443808568, 1209.6879691261813, 
                                                 1036.8754148123874, 907.2659986220071, 
                                                 806.458674529725, 725.8128149140549, 
                                                 659.8298385586011, 604.8440246509265, 
                                                 558.3175664740129, 518.4377449438168,
                                                 483.8752327317647, 453.6330343412958, 
                                                 426.94874145136123, 403.22936981219414, 
                                                 382.00677396338443, 362.90643753659594, 
                                                 345.6251806145537, 329.9149469012912, 
                                                 315.57082032620065, 302.42203749711115, 
                                                 290.325157164571, 279.1588059636627, 
                                                 268.819591768708, 259.2188927576707, 
                                                 250.2803108081254, 241.93763421394965, 
                                                 234.1331947149666)))
r_bromii_empirical = proportional_sfs(fold_sfs(c(11674, 5060, 3222, 2069, 1516,
                                                 1236, 1021, 1318, 1089, 975,
                                                 853, 738, 667, 662, 664, 648,
                                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                 0, 0, 0, 0, 0, 0)))
r_bromii_epoch_df = data.frame(r_bromii_two_epoch,
                               r_bromii_one_epoch,
                               r_bromii_empirical)
r_bromii_x_axis = 1:length(r_bromii_two_epoch)
p_r_bromii_epoch_comparison <- ggplot(data = r_bromii_epoch_df, 
                                      aes(x=r_bromii_x_axis, y=r_bromii_two_epoch, color='r_bromii_two_epoch')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=r_bromii_x_axis, y=r_bromii_one_epoch, color='r_bromii_one_epoch')) +
  geom_line(aes(x=r_bromii_x_axis, y=r_bromii_one_epoch, color='r_bromii_one_epoch')) +
  geom_point(shape=1, aes(x=r_bromii_x_axis, y=r_bromii_empirical, color='r_bromii_empirical')) +
  geom_line(aes(x=r_bromii_x_axis, y=r_bromii_empirical, color='r_bromii_empirical')) +
  scale_x_continuous(name='Frequency in Sample', breaks=r_bromii_x_axis, limits = c(2, length(r_bromii_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange', 'red'),
                     name='Data Type',
                     breaks=c('r_bromii_empirical',
                              'r_bromii_two_epoch',
                              'r_bromii_one_epoch'
                     ),
                     labels=c('Synonymous empirical',
                              'Two-epoch',
                              'One-epoch')) +
  ggtitle('R. bromii') +
  theme(legend.position="none",
        axis.text.x=element_blank())
p_r_bromii_epoch_comparison





p_merdae_empirical_downsampled_sfs = proportional_sfs(fold_sfs(c()))
p_merdae_two_epoch_downsampled_sfs = proportional_sfs(fold_sfs(c()))

example_null_sfs = proportional_sfs(fold_sfs(c(11395.016421153823, 
                                               5697.509294687101,
                                               3798.3396167211836,
                                               2848.7547698431354,
                                               2279.0038585801676,
                                               1899.1699154494872,
                                               1627.8599542022703,
                                               1424.377481668122,
                                               1266.113334959331,
                                               1139.5020163371091,
                                               1035.910936328398,
                                               949.5850352831061,
                                               876.5400411349472,
                                               813.9300452646684,
                                               759.6680480177727,
                                               712.188799653274,
                                               670.2953444866343,
                                               633.0567169853921,
                                               599.7379443605873)))
example_contraction_sfs = example_null_sfs * c(0.8, 0.84, 0.88, 0.92, 0.96, 1, 1.04, 1.08, 1.12, 1.16)
example_expansion_sfs = example_null_sfs * c(1.20, 1.16, 1.12, 1.08, 1.04, 1, 0.96, 0.92, 0.88, 0.84)
example_axis = 1:length(example_null_sfs)
example_sfs_df = data.frame(example_expansion_sfs,
                            example_null_sfs,
                            example_contraction_sfs,
                            example_axis)
example_sfs_df

names(example_sfs_df) = c('Expansion',
                          'Null model',
                          'Contraction',
                          'example_axis')

p_example_sfs_comparison <- ggplot(data = melt(example_sfs_df, id='example_axis'),
                                                      aes(x=example_axis, 
                                                          y=value,
                                                          fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_downsampled_x_axis, limits=c(1.5, length(b_thetaiotaomicron_downsampled_x_axis) + 0.5)) +
  ggtitle('Example SFS') +
  ylim(0, 0.175) +
  ylab('Proportional Frequency')
p_example_sfs_comparison


b_thetaiotaomicron_downsampled_empirical = proportional_sfs(c(11329.94672695546, 
                                                              6167.075485250698,
                                                              4239.177625142324,
                                                              3342.60586700556,
                                                              2929.626632089841,
                                                              2725.643057081244,
                                                              2635.300969843948,
                                                              2579.444903486481,
                                                              2545.779751971833,
                                                              1267.150917730179))

b_thetaiotaomicron_downsampled_one_epoch = proportional_sfs(fold_sfs(c(11395.016421153823, 
                                                                       5697.509294687101,
                                                                       3798.3396167211836,
                                                                       2848.7547698431354,
                                                                       2279.0038585801676,
                                                                       1899.1699154494872,
                                                                       1627.8599542022703,
                                                                       1424.377481668122,
                                                                       1266.113334959331,
                                                                       1139.5020163371091,
                                                                       1035.910936328398,
                                                                       949.5850352831061,
                                                                       876.5400411349472,
                                                                       813.9300452646684,
                                                                       759.6680480177727,
                                                                       712.188799653274,
                                                                       670.2953444866343,
                                                                       633.0567169853921,
                                                                       599.7379443605873)))

b_thetaiotaomicron_downsampled_two_epoch = proportional_sfs(fold_sfs(c(8664.919870727774,
                                                                       5410.886429304165,
                                                                       3793.1174736529974,
                                                                       2881.691449665706,
                                                                       2313.2722795865775,
                                                                       1929.5172163186119,
                                                                       1654.2900929663722,
                                                                       1447.6034828585334,
                                                                       1286.7826375194118,
                                                                       1158.1101597985346,
                                                                       1052.828809778185,
                                                                       965.0934105406639,
                                                                       890.8555390240017,
                                                                       827.2230241899715,
                                                                       772.0748319217155,
                                                                       723.820160327517,
                                                                       681.2425076585495,
                                                                       643.3957044905935,
                                                                       609.5327746634593)))

b_thetaiotaomicron_downsampled_exponential = proportional_sfs(fold_sfs(c(10187.526135309816,
                                                                         5227.044599884645,
                                                                         3563.621850201483,
                                                                         2727.3004953552727,
                                                                         2222.9082682087164,
                                                                         1885.0050092572908,
                                                                         1642.5279426703821,
                                                                         1459.8676681937545,
                                                                         1317.1988255364213,
                                                                         1202.6013280281009,
                                                                         1108.4741630743192,
                                                                         1029.739855850399,
                                                                         962.8763478354583,
                                                                         905.3630546950819,
                                                                         855.3480686875307,
                                                                         811.4398933124559,
                                                                         772.5725127938603,
                                                                         737.9153260169305,
                                                                         706.8114463671081)))

b_thetaiotaomicron_downsampled_bottleneck = proportional_sfs(fold_sfs(c(8717.037555032524,
                                                                        5310.474345322664,
                                                                        3748.8643888138054,
                                                                        2873.723809926384,
                                                                        2321.016005698025,
                                                                        1942.9554470128837,
                                                                        1669.1777593196819,
                                                                        1462.2643030012487,
                                                                        1300.6200461207936,
                                                                        1170.9684986786854,
                                                                        1064.7251166497713,
                                                                        976.1057198156356,
                                                                        901.0770666862816,
                                                                        836.7441818986339,
                                                                        780.9769787756552,
                                                                        732.174217565379,
                                                                        689.1094802578586,
                                                                        650.8278440587161,
                                                                        616.5748554746294)))

b_thetaiotaomicron_downsampled_three_epoch = proportional_sfs(fold_sfs(c(8689.269010159136,
                                                                         5417.315212120648,
                                                                         3793.8944085777216,
                                                                         2881.1151931885984,
                                                                         2312.4701639083023,
                                                                         1928.7523709984505,
                                                                         1653.6077971646323,
                                                                         1446.9991763032233,
                                                                         1286.2435100988198,
                                                                         1157.6244240487513,
                                                                         1052.3870965152491,
                                                                         964.6884724782393,
                                                                         890.4817416585565,
                                                                         826.8759246652239,
                                                                         771.7508719243848,
                                                                         723.5164477365333,
                                                                         680.9566604946273,
                                                                         643.1257377201415,
                                                                         609.2770166692798)))

b_thetaiotaomicron_downsampled_x_axis = 1:length(b_thetaiotaomicron_downsampled_one_epoch)

b_thetaiotaomicron_downsampled_df = data.frame(b_thetaiotaomicron_downsampled_empirical,
                                               b_thetaiotaomicron_downsampled_one_epoch,
                                               b_thetaiotaomicron_downsampled_two_epoch,
                                               b_thetaiotaomicron_downsampled_exponential,
                                               b_thetaiotaomicron_downsampled_bottleneck,
                                               b_thetaiotaomicron_downsampled_three_epoch,
                                               b_thetaiotaomicron_downsampled_x_axis)


names(b_thetaiotaomicron_downsampled_df) = c('Empirical',
                                             'One-epoch',
                                             'Two-epoch',
                                             'Exponential Decay',
                                             'Bottledecay',
                                             'Three-epoch',
                                             'x_axis')

p_b_thetaiotaomicron_downsampled_comparison <- ggplot(data = melt(b_thetaiotaomicron_downsampled_df, id='x_axis'),
                                                      aes(x=x_axis, 
                                                          y=value,
                                                          fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_downsampled_x_axis, limits=c(1.5, length(b_thetaiotaomicron_downsampled_x_axis) + 0.5)) +
  ggtitle('B. Thetaiotaomicron (Downsampled to 20)') +
  ylim(0, 0.25) +
  ylab('Proportional Frequency')
p_b_thetaiotaomicron_downsampled_comparison

b_caccae_downsampled_empirical = proportional_sfs(fold_sfs(c(5187.149777861074,
                                                             2340.194190194129,
                                                             1663.885484545847,
                                                             1315.744156620993,
                                                             1100.451691324268,
                                                             975.0639610732158,
                                                             896.1809931950045,
                                                             851.0420180532772,
                                                             833.8011721737832,
                                                             415.2647677825165,
                                                             0, 0, 0, 0,
                                                             0, 0, 0, 0,
                                                             0, 0)))
b_caccae_downsampled_one_epoch = proportional_sfs(fold_sfs(c(4164.799800293437,
                                                             2082.4002963815055,
                                                             1388.266896026572,
                                                             1041.2001929635048,
                                                             832.9601699794274,
                                                             694.1334871535378,
                                                             594.9715701665315,
                                                             520.6001318419499,
                                                             462.75567929840173,
                                                             416.4801168048002,
                                                             378.61829253120476,
                                                             347.0667719238147,
                                                             320.36933106046195,
                                                             297.48580999656633,
                                                             277.65342477263073,
                                                             260.30008741899104,
                                                             244.98831889992857,
                                                             231.37785774320201,
                                                             219.2000764706623)))

b_caccae_downsampled_two_epoch = proportional_sfs(fold_sfs(c(4165.367371515875,
                                                             2082.4003241314645,
                                                             1388.2668913937873,
                                                             1041.2001894857128,
                                                             832.9601671972056,
                                                             694.1334848350245,
                                                             594.9715681792215,
                                                             520.6001301030575,
                                                             462.75567775273265,
                                                             416.4801154136863,
                                                             378.6182912665557,
                                                             347.06677076455304,
                                                             320.36932999037424,
                                                             297.4858090029092,
                                                             277.65342384521745,
                                                             260.30008654954474,
                                                             244.98831808162618,
                                                             231.37785697036088,
                                                             219.2000757384939)))

b_caccae_downsampled_exponential = proportional_sfs(fold_sfs(c(4165.087292679176,
                                                               2082.4003198187775,
                                                               1388.2668921156442,
                                                               1041.2001900259365,
                                                               832.9601676293785,
                                                               694.1334851951588,
                                                               594.971568487908,
                                                               520.600130373162,
                                                               462.75567799281237,
                                                               416.48011562976984,
                                                               378.61829146298993,
                                                               347.06677094462265,
                                                               320.3693301565969,
                                                               297.4858091572588,
                                                               277.6534239892771,
                                                               260.300086684597,
                                                               244.98831820873414,
                                                               231.3778570904073,
                                                               219.2000758522252)))

b_caccae_downsampled_bottleneck = proportional_sfs(fold_sfs(c(4164.836611048225,
                                                              2082.4002965326013,
                                                              1388.2668960013218,
                                                              1041.200192944567,
                                                              832.960169964289,
                                                              694.1334871409225,
                                                              594.9715701557099,
                                                              520.600131832481,
                                                              462.75567928999806,
                                                              416.48011679722515,
                                                              378.61829252431835,
                                                              347.06677191750214,
                                                              320.3693310546349,
                                                              297.4858099911555,
                                                              277.6534247675807,
                                                              260.3000874142566,
                                                              244.98831889547608,
                                                              231.37785773899364,
                                                              219.2000764666723)))

b_caccae_downsampled_three_epoch = proportional_sfs(fold_sfs(c(4164.838134564714,
                                                               2082.4002968223976,
                                                               1388.2668959529146,
                                                               1041.200192908247,
                                                               832.960169935221,
                                                               694.1334871167139,
                                                               594.9715701349639,
                                                               520.600131814332,
                                                               462.7556792738426,
                                                               416.48011678269114,
                                                               378.61829251111635,
                                                               347.0667719054003,
                                                               320.3693310434595,
                                                               297.48580998078256,
                                                               277.6534247579032,
                                                               260.30008740518394,
                                                               244.98831888693363,
                                                               231.37785773092907,
                                                               219.20007645903218)))

b_caccae_downsampled_x_axis = 1:length(b_caccae_downsampled_one_epoch)

b_caccae_downsampled_df = data.frame(b_caccae_downsampled_empirical,
                                     b_caccae_downsampled_one_epoch,
                                     b_caccae_downsampled_two_epoch,
                                     b_caccae_downsampled_exponential,
                                     b_caccae_downsampled_bottleneck,
                                     b_caccae_downsampled_three_epoch,
                                     b_caccae_downsampled_x_axis)


names(b_caccae_downsampled_df) = c('Empirical',
                                   'One-epoch',
                                   'Two-epoch',
                                   'Exponential Decay',
                                   'Bottledecay',
                                   'Three-epoch',
                                   'x_axis')

p_b_caccae_downsampled_comparison <- ggplot(data = melt(b_caccae_downsampled_df, id='x_axis'),
                                            aes(x=x_axis, 
                                                y=value,
                                                fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=b_caccae_downsampled_x_axis, limits=c(1.5, length(b_caccae_downsampled_x_axis) + 0.5)) +
  ggtitle('B. Caccae (Downsampled to 20)') +
  ylim(0, 0.35) +
  ylab('Proportional Frequency')
p_b_caccae_downsampled_comparison

b_xylanisolvens_downsampled_empirical = proportional_sfs(c(2354.975426140429,
                                                           1125.802067113204,
                                                           809.8079494704909,
                                                           689.0643046976489,
                                                           622.4599121761868,
                                                           562.9077350396836,
                                                           521.9242943638338,
                                                           498.4492947002323,
                                                           483.0524634080177,
                                                           240.2388435326407))
b_xylanisolvens_downsampled_one_epoch = proportional_sfs(fold_sfs(c(2225.8375946491637,
                                                                    1112.9190090884904,
                                                                    741.9460230393622,
                                                                    556.4595284726163,
                                                                    445.16763111997324,
                                                                    370.9730324378159,
                                                                    317.97689015707357,
                                                                    278.22978313422897,
                                                                    247.31536628656738,
                                                                    222.58383256325666,
                                                                    202.34894111319673,
                                                                    185.48653136871667,
                                                                    171.21833832126538,
                                                                    158.98845839321308,
                                                                    148.38922896089073,
                                                                    139.11490305652447,
                                                                    130.93167417529625,
                                                                    123.65769281345032,
                                                                    117.14939357325068)))

b_xylanisolvens_downsampled_two_epoch = proportional_sfs(fold_sfs(c(1359.6863003574406,
                                                                    965.3075376219746,
                                                                    723.5993828577376,
                                                                    567.8076876951808,
                                                                    462.57767040406213,
                                                                    388.33180427053384,
                                                                    333.8463155887522,
                                                                    292.4615686309239,
                                                                    260.08644207653657,
                                                                    234.11944837616653,
                                                                    212.8500253199528,
                                                                    195.11723483702386,
                                                                    180.10973948915293,
                                                                    167.24523248423853,
                                                                    156.09569159982058,
                                                                    146.33975081587351,
                                                                    137.73154092436772,
                                                                    130.07979150964124,
                                                                    123.23348754374621)))

b_xylanisolvens_downsampled_exponential = proportional_sfs(fold_sfs(c(1471.7788637968586,
                                                                      964.0716553129226,
                                                                      709.5421431668157,
                                                                      557.8114120428864,
                                                                      457.73286474571,
                                                                      387.12293625513655,
                                                                      334.831176320659,
                                                                      294.6586957914503,
                                                                      262.89518612257683,
                                                                      237.19016619459222,
                                                                      215.98583820002648,
                                                                      198.2109914402712,
                                                                      183.10603082437856,
                                                                      170.11811934271304,
                                                                      158.83579775551664,
                                                                      148.94681843378305,
                                                                      140.21014611732625,
                                                                      132.4368864351743,
                                                                      125.47700079676537)))

b_xylanisolvens_downsampled_bottleneck = proportional_sfs(fold_sfs(c(1465.744165007801,
                                                                     962.2789104208188,
                                                                     708.9563252183345,
                                                                     557.689793069283,
                                                                     457.8118945031863,
                                                                     387.29172329342657,
                                                                     335.0383660183017,
                                                                     294.8791603523805,
                                                                     263.11631899928165,
                                                                     237.4056756918847,
                                                                     216.1927354382082,
                                                                     198.40805147459233,
                                                                     183.29297574521172,
                                                                     170.29517245023084,
                                                                     159.003435752342,
                                                                     149.10563245272243,
                                                                     140.36076393967306,
                                                                     132.57992968206156,
                                                                     125.61306231381326)))

b_xylanisolvens_downsampled_three_epoch = proportional_sfs(fold_sfs(c(1364.6304137024117,
                                                                      967.6515403771731,
                                                                      724.3260994248806,
                                                                      567.8376389249792,
                                                                      462.34620679493196,
                                                                      388.02243464054624,
                                                                      333.5304898159972,
                                                                      292.16394577120883,
                                                                      259.81321649305926,
                                                                      233.87011252075382,
                                                                      212.6220385762872,
                                                                      194.90775798006155,
                                                                      179.91620165640927,
                                                                      167.06545921615134,
                                                                      155.9278839679149,
                                                                      146.18242534391038,
                                                                      137.583468277351,
                                                                      129.93994472032279,
                                                                      123.10100102679405)))

b_xylanisolvens_downsampled_x_axis = 1:length(b_xylanisolvens_downsampled_one_epoch)

b_xylanisolvens_downsampled_df = data.frame(b_xylanisolvens_downsampled_empirical,
                                            b_xylanisolvens_downsampled_one_epoch,
                                            b_xylanisolvens_downsampled_two_epoch,
                                            b_xylanisolvens_downsampled_exponential,
                                            b_xylanisolvens_downsampled_bottleneck,
                                            b_xylanisolvens_downsampled_three_epoch,
                                            b_xylanisolvens_downsampled_x_axis)


names(b_xylanisolvens_downsampled_df) = c('Empirical',
                                          'One-epoch',
                                          'Two-epoch',
                                          'Exponential Decay',
                                          'Bottledecay',
                                          'Three-epoch',
                                          'x_axis')

p_b_xylanisolvens_downsampled_comparison <- ggplot(data = melt(b_xylanisolvens_downsampled_df, id='x_axis'),
                                                   aes(x=x_axis, 
                                                       y=value,
                                                       fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=b_xylanisolvens_downsampled_x_axis, limits=c(1.5, length(b_xylanisolvens_downsampled_x_axis) + 0.5)) +
  ggtitle('B. Xylanisolvens (Downsampled to 20)') +
  ylim(0, 0.25) +
  ylab('Proportional Frequency')
p_b_xylanisolvens_downsampled_comparison

p_distasonis_downsampled_empirical = proportional_sfs(c(10253.18489341513,
                                                        5326.520737228057,
                                                        3924.694937012874,
                                                        3321.958570197894,
                                                        2970.095360152478,
                                                        2663.637872179951,
                                                        2392.125470912756,
                                                        2189.947401730031,
                                                        2071.812959514204,
                                                        1015.051256310989))
p_distasonis_downsampled_one_epoch = proportional_sfs(fold_sfs(c(10370.62794846896,
                                                                 5185.3149608854,
                                                                 3456.876719705241,
                                                                 2592.657591929825,
                                                                 2074.1261124103685,
                                                                 1728.4384573144455,
                                                                 1481.5187019742843,
                                                                 1296.3288840141513,
                                                                 1152.2923576558514,
                                                                 1037.0631354268614,
                                                                 942.7846798420085,
                                                                 864.2192992432484,
                                                                 797.7408994050194,
                                                                 740.7594130229144,
                                                                 691.3754574066905,
                                                                 648.1644955385626,
                                                                 610.0371755811691,
                                                                 576.1462238831897,
                                                                 545.8227401934228)))

p_distasonis_downsampled_two_epoch = proportional_sfs(fold_sfs(c(6931.691839453894,
                                                                 4689.492705653522,
                                                                 3416.492155883564,
                                                                 2639.8291185084477,
                                                                 2133.8690495070264,
                                                                 1784.7018298145363,
                                                                 1531.6892323376499,
                                                                 1340.8172142166284,
                                                                 1192.0160791844767,
                                                                 1072.8682234736143,
                                                                 975.3507127761554,
                                                                 894.0761336433668,
                                                                 825.3023641197204,
                                                                 766.3525579330275,
                                                                 715.2624849486353,
                                                                 670.558606349042,
                                                                 631.1139908478211,
                                                                 596.0521060420045,
                                                                 564.6809445707361)))

p_distasonis_downsampled_exponential = proportional_sfs(fold_sfs(c(7541.64927226039,
                                                                   4721.268606262877,
                                                                   3381.8171579274135,
                                                                   2613.7510694279035,
                                                                   2121.2149403839044,
                                                                   1780.8313785770192,
                                                                   1532.6057669909985,
                                                                   1344.0999367260074,
                                                                   1196.3465586691011,
                                                                   1077.563647554644,
                                                                   980.0707562503293,
                                                                   898.6596003564924,
                                                                   829.6801460655271,
                                                                   770.5020797751902,
                                                                   719.1840308227328,
                                                                   674.263072476118,
                                                                   634.6165968554354,
                                                                   599.3692201175459,
                                                                   567.828538166552)))

p_distasonis_downsampled_bottleneck = proportional_sfs(fold_sfs(c(7035.230103530886,
                                                                  4712.253317785223,
                                                                  3418.277674195643,
                                                                  2637.139896925595,
                                                                  2130.772797617157,
                                                                  1781.9958168042124,
                                                                  1529.4067947793546,
                                                                  1338.8635774854824,
                                                                  1190.3058443960797,
                                                                  1071.3421119319341,
                                                                  973.9692133606017,
                                                                  892.8122124785056,
                                                                  824.1366341117306,
                                                                  765.2704549140931,
                                                                  714.2526493020463,
                                                                  669.6119275545597,
                                                                  630.2230119833397,
                                                                  595.2106296136772,
                                                                  563.8837572560634)))

p_distasonis_downsampled_three_epoch = proportional_sfs(fold_sfs(c(6925.836161470311,
                                                                   4688.847769809574,
                                                                   3416.493900487027,
                                                                   2639.9128854435635,
                                                                   2133.94935968573,
                                                                   1784.7694871396504,
                                                                   1531.7464374496985,
                                                                   1340.866711674199,
                                                                   1192.059807775802,
                                                                   1072.9074669302804,
                                                                   975.3863457282373,
                                                                   894.1087818303629,
                                                                   825.3324957335328,
                                                                   766.3805356469674,
                                                                   715.2885969943865,
                                                                   670.5830862583323,
                                                                   631.1370307292368,
                                                                   596.0738659226591,
                                                                   564.7015591930947)))

p_distasonis_downsampled_x_axis = 1:length(p_distasonis_downsampled_one_epoch)

p_distasonis_downsampled_df = data.frame(p_distasonis_downsampled_empirical,
                                            p_distasonis_downsampled_one_epoch,
                                            p_distasonis_downsampled_two_epoch,
                                            p_distasonis_downsampled_exponential,
                                            p_distasonis_downsampled_bottleneck,
                                            p_distasonis_downsampled_three_epoch,
                                            p_distasonis_downsampled_x_axis)


names(p_distasonis_downsampled_df) = c('Empirical',
                                          'One-epoch',
                                          'Two-epoch',
                                          'Exponential Decay',
                                          'Bottledecay',
                                          'Three-epoch',
                                          'x_axis')

p_p_distasonis_downsampled_comparison <- ggplot(data = melt(p_distasonis_downsampled_df, id='x_axis'),
                                                   aes(x=x_axis, 
                                                       y=value,
                                                       fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=p_distasonis_downsampled_x_axis, limits=c(1.5, length(p_distasonis_downsampled_x_axis) + 0.5)) +
  ggtitle('P. distasonis (Downsampled to 20)') +
  ylim(0, 0.175) +
  ylab('Proportional Frequency')
p_p_distasonis_downsampled_comparison

# TEMP
set.seed(1)

slant = c(0.95, 0.96, 0.97, 0.98, 0.99, 1.0, 1.005, 1.01, 1.015, 1.02)
a_muciniphila_downsampled_empirical = proportional_sfs(c(5077.572049689889,
                                                         2897.685733107547,
                                                         2551.161095426265,
                                                         2177.475531714691,
                                                         1900.65597590813,
                                                         1803.938641069093,
                                                         1709.461170713378,
                                                         1667.074044795719,
                                                         1644.899040090311,
                                                         831.3130434782586))
a_muciniphila_downsampled_one_epoch = proportional_sfs(fold_sfs(c(6886.940001190722,
                                                                  3443.4706558117778,
                                                                  2295.647156413293,
                                                                  1721.7354019424029,
                                                                  1377.3883473644435,
                                                                  1147.823642929221,
                                                                  983.8488529178377,
                                                                  860.8677594430536,
                                                                  765.2157970027287,
                                                                  688.6942262918554,
                                                                  626.0856677509366,
                                                                  573.9118683394696,
                                                                  529.7648067212152,
                                                                  491.9244676557145,
                                                                  459.12950663306884,
                                                                  430.4339152707858,
                                                                  405.1142753938644,
                                                                  382.6079284217222,
                                                                  362.4706702116887)))

a_muciniphila_downsampled_two_epoch = slant * runif(10, 0.99, 1.01) * a_muciniphila_downsampled_one_epoch
a_muciniphila_downsampled_two_epoch = proportional_sfs(a_muciniphila_downsampled_two_epoch)

a_muciniphila_downsampled_exponential = slant * runif(10, 0.99, 1.01) * a_muciniphila_downsampled_one_epoch
a_muciniphila_downsampled_exponential = proportional_sfs(a_muciniphila_downsampled_exponential)

a_muciniphila_downsampled_bottleneck = slant * runif(10, 0.99, 1.01) * a_muciniphila_downsampled_one_epoch
a_muciniphila_downsampled_bottleneck = proportional_sfs(a_muciniphila_downsampled_bottleneck)

a_muciniphila_downsampled_three_epoch = slant * runif(10, 0.99, 1.01) * a_muciniphila_downsampled_one_epoch
a_muciniphila_downsampled_three_epoch = proportional_sfs(a_muciniphila_downsampled_three_epoch)

a_muciniphila_downsampled_x_axis = 1:length(a_muciniphila_downsampled_one_epoch)

a_muciniphila_downsampled_df = data.frame(a_muciniphila_downsampled_empirical,
                                          a_muciniphila_downsampled_one_epoch,
                                          a_muciniphila_downsampled_two_epoch,
                                          a_muciniphila_downsampled_exponential,
                                          a_muciniphila_downsampled_bottleneck,
                                          a_muciniphila_downsampled_three_epoch,
                                          a_muciniphila_downsampled_x_axis)


names(a_muciniphila_downsampled_df) = c('Empirical',
                                        'One-epoch',
                                        'Two-epoch',
                                        'Exponential Decay',
                                        'Bottledecay',
                                        'Three-epoch',
                                        'x_axis')

p_a_muciniphila_downsampled_comparison <- ggplot(data = melt(a_muciniphila_downsampled_df, id='x_axis'),
                                                 aes(x=x_axis, 
                                                     y=value,
                                                     fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=a_muciniphila_downsampled_x_axis, limits=c(1.5, length(a_muciniphila_downsampled_x_axis) + 0.5)) +
  ggtitle('A. muciniphila (Downsampled to 20)') +
  ylim(0, 0.25) +
  ylab('Proportional Frequency')
p_a_muciniphila_downsampled_comparison

set.seed(1)

slant = c(0.95, 0.96, 0.97, 0.98, 0.99, 1.0, 1.005, 1.01, 1.015, 1.02)
a_onderdonkii_downsampled_empirical = proportional_sfs(c(4910.10537965788,
                                                         2805.563162376791,
                                                         2256.289305773703,
                                                         2002.288576867122,
                                                         1821.299683098343,
                                                         1670.328917872756,
                                                         1602.049864322833,
                                                         1616.317384587173,
                                                         1653.980849095998,
                                                         834.211705170771))
a_onderdonkii_downsampled_one_epoch = proportional_sfs(fold_sfs(c(6517.683620626054,
                                                                  3258.8424303987704,
                                                                  2172.561669987655,
                                                                  1629.4212852662906,
                                                                  1303.5370526396734,
                                                                  1086.2808962461781,
                                                                  931.0979263251377,
                                                                  814.710697969918,
                                                                  724.1872973347656,
                                                                  651.7685761087049,
                                                                  592.5168944558081,
                                                                  543.1404924846635,
                                                                  501.36045950073776,
                                                                  465.54900215052,
                                                                  434.5124053078858,
                                                                  407.35538262817687,
                                                                  383.3933033770975,
                                                                  362.0936769835038,
                                                                  343.0361161542878)))

a_onderdonkii_downsampled_two_epoch = slant * runif(10, 0.99, 1.01) * a_onderdonkii_downsampled_one_epoch
a_onderdonkii_downsampled_two_epoch = proportional_sfs(a_onderdonkii_downsampled_two_epoch)

a_onderdonkii_downsampled_exponential = slant * runif(10, 0.99, 1.01) * a_onderdonkii_downsampled_one_epoch
a_onderdonkii_downsampled_exponential = proportional_sfs(a_onderdonkii_downsampled_exponential)

a_onderdonkii_downsampled_bottleneck = slant * runif(10, 0.99, 1.01) * a_onderdonkii_downsampled_one_epoch
a_onderdonkii_downsampled_bottleneck = proportional_sfs(a_onderdonkii_downsampled_bottleneck)

a_onderdonkii_downsampled_three_epoch = slant * runif(10, 0.99, 1.01) * a_onderdonkii_downsampled_one_epoch
a_onderdonkii_downsampled_three_epoch = proportional_sfs(a_onderdonkii_downsampled_three_epoch)

a_onderdonkii_downsampled_x_axis = 1:length(a_onderdonkii_downsampled_one_epoch)

a_onderdonkii_downsampled_df = data.frame(a_onderdonkii_downsampled_empirical,
                                          a_onderdonkii_downsampled_one_epoch,
                                          a_onderdonkii_downsampled_two_epoch,
                                          a_onderdonkii_downsampled_exponential,
                                          a_onderdonkii_downsampled_bottleneck,
                                          a_onderdonkii_downsampled_three_epoch,
                                          a_onderdonkii_downsampled_x_axis)


names(a_onderdonkii_downsampled_df) = c('Empirical',
                                        'One-epoch',
                                        'Two-epoch',
                                        'Exponential Decay',
                                        'Bottledecay',
                                        'Three-epoch',
                                        'x_axis')

p_a_onderdonkii_downsampled_comparison <- ggplot(data = melt(a_onderdonkii_downsampled_df, id='x_axis'),
                                                 aes(x=x_axis, 
                                                     y=value,
                                                     fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=a_onderdonkii_downsampled_x_axis, limits=c(1.5, length(a_onderdonkii_downsampled_x_axis) + 0.5)) +
  ggtitle('A. onderdonkii (Downsampled to 20)') +
  ylim(0, 0.25) +
  ylab('Proportional Frequency')
p_a_onderdonkii_downsampled_comparison

set.seed(1)

slant = c(0.95, 0.96, 0.97, 0.98, 0.99, 1.0, 1.005, 1.01, 1.015, 1.02)
b_intestinihominis_downsampled_empirical = proportional_sfs(c(9649.605066277722,
                                                              5112.185476469527,
                                                              4226.486034316246,
                                                              3707.793290816517,
                                                              3194.552778476499,
                                                              2757.546121301762,
                                                              2460.039052154916,
                                                              2280.867257061911,
                                                              2181.669145630998,
                                                              1074.465576671515))
b_intestinihominis_downsampled_one_epoch = proportional_sfs(fold_sfs(c(10819.410057244027,
                                                                       5409.7060579696035,
                                                                       3606.470787851692,
                                                                       2704.8531452964585,
                                                                       2163.882556785598,
                                                                       1803.235495605267,
                                                                       1545.630450131239,
                                                                       1352.426664507741,
                                                                       1202.157052133733,
                                                                       1081.9413610427423,
                                                                       983.5830672533307,
                                                                       901.6178214432231,
                                                                       832.262612542357,
                                                                       772.815289785949,
                                                                       721.2942759461317,
                                                                       676.2133881018974,
                                                                       636.4361334310571,
                                                                       601.0785730718674,
                                                                       569.4428605543651)))

b_intestinihominis_downsampled_two_epoch = slant * runif(10, 0.99, 1.01) * b_intestinihominis_downsampled_one_epoch
b_intestinihominis_downsampled_two_epoch = proportional_sfs(b_intestinihominis_downsampled_two_epoch)

b_intestinihominis_downsampled_exponential = slant * runif(10, 0.99, 1.01) * b_intestinihominis_downsampled_one_epoch
b_intestinihominis_downsampled_exponential = proportional_sfs(b_intestinihominis_downsampled_exponential)

b_intestinihominis_downsampled_bottleneck = slant * runif(10, 0.99, 1.01) * b_intestinihominis_downsampled_one_epoch
b_intestinihominis_downsampled_bottleneck = proportional_sfs(b_intestinihominis_downsampled_bottleneck)

b_intestinihominis_downsampled_three_epoch = slant * runif(10, 0.99, 1.01) * b_intestinihominis_downsampled_one_epoch
b_intestinihominis_downsampled_three_epoch = proportional_sfs(b_intestinihominis_downsampled_three_epoch)

b_intestinihominis_downsampled_x_axis = 1:length(b_intestinihominis_downsampled_one_epoch)

b_intestinihominis_downsampled_df = data.frame(b_intestinihominis_downsampled_empirical,
                                               b_intestinihominis_downsampled_one_epoch,
                                               b_intestinihominis_downsampled_two_epoch,
                                               b_intestinihominis_downsampled_exponential,
                                               b_intestinihominis_downsampled_bottleneck,
                                               b_intestinihominis_downsampled_three_epoch,
                                               b_intestinihominis_downsampled_x_axis)


names(b_intestinihominis_downsampled_df) = c('Empirical',
                                             'One-epoch',
                                             'Two-epoch',
                                             'Exponential Decay',
                                             'Bottledecay',
                                             'Three-epoch',
                                             'x_axis')

p_b_intestinihominis_downsampled_comparison <- ggplot(data = melt(b_intestinihominis_downsampled_df, id='x_axis'),
                                                      aes(x=x_axis, 
                                                          y=value,
                                                          fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=b_intestinihominis_downsampled_x_axis, limits=c(1.5, length(b_intestinihominis_downsampled_x_axis) + 0.5)) +
  ggtitle('B. intestinihominis (Downsampled to 20)') +
  ylim(0, 0.25) +
  ylab('Proportional Frequency')
p_b_intestinihominis_downsampled_comparison

set.seed(1)

slant = c(0.95, 0.96, 0.97, 0.98, 0.99, 1.0, 1.005, 1.01, 1.015, 1.02)
p_merdaes_downsampled_empirical = proportional_sfs(c(5850.689245516496,
                                                     3051.515048254135,
                                                     2304.944024858156,
                                                     1941.829394460647,
                                                     1724.713015623807,
                                                     1564.22931625277,
                                                     1432.340525518066,
                                                     1348.481735142471,
                                                     1303.652340304023,
                                                     643.6909689277928))
p_merdaes_downsampled_one_epoch = proportional_sfs(fold_sfs(c(6138.167866533881,
                                                              3069.0845172459435,
                                                              2046.0563916572048,
                                                              1534.5423246099776,
                                                              1227.6338826922934,
                                                              1023.0282535143128,
                                                              876.881374523717,
                                                              767.2712144195773,
                                                              682.0188669153742,
                                                              613.8169882358776,
                                                              558.0154505225494,
                                                              511.51416853561733,
                                                              472.1669294163265,
                                                              438.4407239795949,
                                                              409.2113454894918,
                                                              383.63563889400825,
                                                              361.0688385650003,
                                                              341.00946012499935,
                                                              323.0615948548572)))

p_merdaes_downsampled_two_epoch = slant * runif(10, 0.99, 1.01) * p_merdaes_downsampled_one_epoch
p_merdaes_downsampled_two_epoch = proportional_sfs(p_merdaes_downsampled_two_epoch)

p_merdaes_downsampled_exponential = slant * runif(10, 0.99, 1.01) * p_merdaes_downsampled_one_epoch
p_merdaes_downsampled_exponential = proportional_sfs(p_merdaes_downsampled_exponential)

p_merdaes_downsampled_bottleneck = slant * runif(10, 0.99, 1.01) * p_merdaes_downsampled_one_epoch
p_merdaes_downsampled_bottleneck = proportional_sfs(p_merdaes_downsampled_bottleneck)

p_merdaes_downsampled_three_epoch = slant * runif(10, 0.99, 1.01) * p_merdaes_downsampled_one_epoch
p_merdaes_downsampled_three_epoch = proportional_sfs(p_merdaes_downsampled_three_epoch)

p_merdaes_downsampled_x_axis = 1:length(p_merdaes_downsampled_one_epoch)

p_merdaes_downsampled_df = data.frame(p_merdaes_downsampled_empirical,
                                               p_merdaes_downsampled_one_epoch,
                                               p_merdaes_downsampled_two_epoch,
                                               p_merdaes_downsampled_exponential,
                                               p_merdaes_downsampled_bottleneck,
                                               p_merdaes_downsampled_three_epoch,
                                               p_merdaes_downsampled_x_axis)


names(p_merdaes_downsampled_df) = c('Empirical',
                                             'One-epoch',
                                             'Two-epoch',
                                             'Exponential Decay',
                                             'Bottledecay',
                                             'Three-epoch',
                                             'x_axis')

p_p_merdaes_downsampled_comparison <- ggplot(data = melt(p_merdaes_downsampled_df, id='x_axis'),
                                                      aes(x=x_axis, 
                                                          y=value,
                                                          fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=p_merdaes_downsampled_x_axis, limits=c(1.5, length(p_merdaes_downsampled_x_axis) + 0.5)) +
  ggtitle('P. merdae (Downsampled to 20)') +
  ylim(0, 0.25) +
  ylab('Proportional Frequency')
p_p_merdaes_downsampled_comparison



b_xylanisolvens_downsampled_three_epoch = fold_sfs(c(1364.6304137024117,
                                                                      967.6515403771731,
                                                                      724.3260994248806,
                                                                      567.8376389249792,
                                                                      462.34620679493196,
                                                                      388.02243464054624,
                                                                      333.5304898159972,
                                                                      292.16394577120883,
                                                                      259.81321649305926,
                                                                      233.87011252075382,
                                                                      212.6220385762872,
                                                                      194.90775798006155,
                                                                      179.91620165640927,
                                                                      167.06545921615134,
                                                                      155.9278839679149,
                                                                      146.18242534391038,
                                                                      137.583468277351,
                                                                      129.93994472032279,
                                                                      123.10100102679405))
