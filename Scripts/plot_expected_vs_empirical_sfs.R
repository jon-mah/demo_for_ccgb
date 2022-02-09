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
    output_sfs[i] = input_sfs[i] + input_sfs[input_length - i]
  }
  return(output_sfs)
}

proportional_sfs = function(input_sfs) {
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

df = data.frame(b_vulgatus_empirical_syn_no_s_sfs, b_vulgatus_bottleneck_growth_syn_no_s_sfs, b_vulgatus_exponential_growth_syn_no_s_sfs, b_vulgatus_two_epoch_syn_no_s_sfs, b_vulgatus_three_epoch_syn_no_s_sfs)

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

a_onderdonkii_empirical = proportional_sfs(fold_sfs(c(5437, 2738, 1710, 1383, 1243, 1033, 1095, 983, 1011, 899, 887, 754, 681, 668, 567, 424, 480, 476, 509,
                                                      512, 615, 557, 605, 586, 476, 389, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                      0, 0, 0)))
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

a_onderdonkii_df = data.frame(a_onderdonkii_empirical,
                             a_onderdonkii_one_epoch,
                             a_onderdonkii_two_epoch)
a_onderdonkii_x_axis = 1:length(a_onderdonkii_empirical)
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
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('a_onderdonkii_empirical',
                              'a_onderdonkii_one_epoch',
                              'a_onderdonkii_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('A. onderdonkii')
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
                              'Two epoch')) +
  ggtitle('B. intestinihominis')
p_b_intestinihominis_comparison

b_thetaiotamicron_empirical = proportional_sfs(fold_sfs(c(9825, 5001, 3405, 3070, 2406, 2519, 1867, 1646, 1485, 1256, 1141, 1144,
                                                          1067, 994, 930, 927, 833, 934, 824, 834, 817, 810, 820, 760, 697, 733,
                                                          705, 736, 809, 686, 612, 485, 228, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
b_thetaiotamicron_two_epoch = proportional_sfs(fold_sfs(c(6883.161202897391, 4741.59509909963, 3513.1941264046495, 2747.44614715245,
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
b_thetaiotamicron_one_epoch = proportional_sfs(fold_sfs(c(10999.512256132666, 5499.759102479669, 3666.5061376191825, 2749.879627802355,
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

b_thetaiotamicron_df = data.frame(b_thetaiotamicron_empirical,
                             b_thetaiotamicron_one_epoch,
                             b_thetaiotamicron_two_epoch)
b_thetaiotamicron_x_axis = 1:length(b_thetaiotamicron_empirical)
p_b_thetaiotamicron_comparison <- ggplot(data = b_thetaiotamicron_df, aes(x=b_thetaiotamicron_x_axis, y=b_thetaiotamicron_empirical, color='b_thetaiotamicron_empirical')) +
  geom_point(shape=1) +
  geom_line() +
  geom_point(shape=1, aes(x=b_thetaiotamicron_x_axis, y=b_thetaiotamicron_empirical, color='b_thetaiotamicron_empirical')) +
  geom_line(aes(x=b_thetaiotamicron_x_axis, y=b_thetaiotamicron_empirical, color='b_thetaiotamicron_empirical')) +  
  geom_point(shape=1, aes(x=b_thetaiotamicron_x_axis, y=b_thetaiotamicron_one_epoch, color='b_thetaiotamicron_one_epoch')) +
  geom_line(aes(x=b_thetaiotamicron_x_axis, y=b_thetaiotamicron_one_epoch, color='b_thetaiotamicron_one_epoch')) +
  geom_point(shape=1, aes(x=b_thetaiotamicron_x_axis, y=b_thetaiotamicron_two_epoch, color='b_thetaiotamicron_two_epoch')) +
  geom_line(aes(x=b_thetaiotamicron_x_axis, y=b_thetaiotamicron_two_epoch, color='b_thetaiotamicron_two_epoch')) +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotamicron_x_axis, limits = c(2, length(b_thetaiotamicron_x_axis))) +
  scale_y_continuous(name='SNP proportion') +
  scale_color_manual(values=c('black', 'blue', 'orange'),
                     name='Data Type',
                     breaks=c('b_thetaiotamicron_empirical',
                              'b_thetaiotamicron_one_epoch',
                              'b_thetaiotamicron_two_epoch'
                     ),
                     labels=c('Empirical',
                              'One epoch',
                              'Two epoch')) +
  ggtitle('B. thetaiotamicron')
p_b_thetaiotamicron_comparison

p_distasonis_empirical = proportional_sfs(fold_sfs(c(8721, 4302, 2876, 2723, 2110, 1975, 1740, 1781, 1609, 1586, 1292, 1140, 1108,
                                                     993, 972, 924, 861, 837, 834, 696, 687, 638, 583, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
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

p_distasonis_df = data.frame(p_distasonis_empirical,
                             p_distasonis_one_epoch,
                             p_distasonis_two_epoch)
p_distasonis_x_axis = 1:length(p_distasonis_empirical)
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
                              'Two epoch')) +
  ggtitle('P. distasonis')
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