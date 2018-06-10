int n
int v
int w
list list list int w_
list list list int w__

l0: entry
  skip
goto l1

l1: from l0
  init w__ [88,88,3]
goto l14

l14: from l1
  skip
goto l15

l15: from l14
  skip
if (null w__) l16 l18

l16: from l15
  skip
goto l17

l17: from l16
  skip
goto l12

l18: from l15
  skip
goto l19

l19: from l18
  skip
goto l12

l12: fi (1) l17 l19
  skip
goto l13

l13: from l12
  w__[0,1,0] ^= 1
goto l10

l10: from l13
  skip
goto l11

l11: fi (w__[0,1,0]) l10 l23
  skip
goto l20

l20: from l11
  skip
if (w__[0,1,0]) l24 l26

l24: from l20
  swap w__[0,1,0] w__[1,1,1]
goto l25

l25: from l24
  skip
goto l21

l26: from l20
  skip
if (w__[1,1,1]) l28 l30

l28: from l26
  skip
goto l32

l32: from l28
  skip
goto l33

l33: from l32
  swap w__[1,1,1] w__[1,1,2]
goto l29

l29: from l33
  skip
goto l27

l30: from l26
  skip
if (w__[1,1,2]) l34 l36

l34: from l30
  swap w__[1,1,2] w__[1,2,0]
goto l35

l35: from l34
  skip
goto l31

l36: from l30
  skip
if (w__[1,2,0]) l38 l40

l38: from l36
  swap w__[1,2,0] w__[2,2,1]
goto l39

l39: from l38
  skip
goto l37

l40: from l36
  skip
if (w__[2,2,1]) l42 l44

l42: from l40
  init w_ [5,5,3]
goto l46

l46: from l42
  skip
goto l47

l47: from l46
  swap w__[2,2,1] w__[2,2,2]
goto l43

l43: from l47
  skip
goto l41

l44: from l40
  skip
if (w__[2,2,2]) l48 l50

l48: from l44
  swap w__[2,2,2] w__[2,3,0]
goto l49

l49: from l48
  skip
goto l45

l50: from l44
  skip
if (w__[2,3,0]) l52 l54

l52: from l50
  swap w__[2,3,0] w__[3,3,1]
goto l53

l53: from l52
  skip
goto l51

l54: from l50
  skip
if (w__[3,3,1]) l56 l58

l56: from l54
  skip
goto l60

l60: from l56
  skip
goto l61

l61: from l60
  swap w__[3,3,1] w__[3,3,2]
goto l57

l57: from l61
  skip
goto l55

l58: from l54
  skip
if (w__[3,3,2]) l62 l64

l62: from l58
  swap w__[3,3,2] w__[3,4,0]
goto l63

l63: from l62
  skip
goto l59

l64: from l58
  skip
if (w__[3,4,0]) l66 l68

l66: from l64
  swap w__[3,4,0] w__[4,4,1]
goto l67

l67: from l66
  skip
goto l65

l68: from l64
  skip
if (w__[4,4,1]) l70 l72

l70: from l68
  skip
goto l74

l74: from l70
  skip
goto l75

l75: from l74
  swap w__[4,4,1] w__[4,4,2]
goto l71

l71: from l75
  skip
goto l69

l72: from l68
  skip
if (w__[4,4,2]) l76 l78

l76: from l72
  skip
if (null w_) l80 l82

l80: from l76
  swap w__[4,4,2] w__[4,5,0]
goto l81

l81: from l80
  skip
goto l77

l82: from l76
  swap w__[4,4,2] w__[4,7,0]
goto l83

l83: from l82
  skip
goto l77

l77: fi (w__[4,5,0]) l81 l83
  skip
goto l73

l78: from l72
  skip
if (w__[4,5,0]) l84 l86

l84: from l78
  swap w__[4,5,0] w__[5,5,1]
goto l85

l85: from l84
  skip
goto l79

l86: from l78
  skip
if (w__[5,5,1]) l88 l90

l88: from l86
  skip
goto l92

l92: from l88
  skip
goto l93

l93: from l92
  swap w__[5,5,1] w__[5,5,2]
goto l89

l89: from l93
  skip
goto l87

l90: from l86
  skip
if (w__[5,5,2]) l94 l96

l94: from l90
  swap w__[5,5,2] w__[5,6,0]
goto l95

l95: from l94
  skip
goto l91

l96: from l90
  skip
if (w__[5,6,0]) l98 l100

l98: from l96
  swap w__[5,6,0] w__[6,6,1]
goto l99

l99: from l98
  skip
goto l97

l100: from l96
  skip
if (w__[6,6,1]) l102 l104

l102: from l100
  skip
goto l106

l106: from l102
  skip
goto l107

l107: from l106
  swap w__[6,6,1] w__[6,6,2]
goto l103

l103: from l107
  skip
goto l101

l104: from l100
  skip
if (w__[6,6,2]) l108 l110

l108: from l104
  swap w__[6,6,2] w__[6,9,0]
goto l109

l109: from l108
  skip
goto l105

l110: from l104
  skip
if (w__[4,7,0]) l112 l114

l112: from l110
  swap w__[4,7,0] w__[7,7,1]
goto l113

l113: from l112
  skip
goto l111

l114: from l110
  skip
if (w__[7,7,1]) l116 l118

l116: from l114
  skip
goto l120

l120: from l116
  skip
goto l121

l121: from l120
  swap w__[7,7,1] w__[7,7,2]
goto l117

l117: from l121
  skip
goto l115

l118: from l114
  skip
if (w__[7,7,2]) l122 l124

l122: from l118
  swap w__[7,7,2] w__[7,8,0]
goto l123

l123: from l122
  skip
goto l119

l124: from l118
  skip
if (w__[7,8,0]) l126 l128

l126: from l124
  swap w__[7,8,0] w__[8,8,1]
goto l127

l127: from l126
  skip
goto l125

l128: from l124
  skip
if (w__[8,8,1]) l130 l132

l130: from l128
  skip
goto l134

l134: from l130
  skip
goto l135

l135: from l134
  swap w__[8,8,1] w__[8,8,2]
goto l131

l131: from l135
  skip
goto l129

l132: from l128
  skip
if (w__[8,8,2]) l136 l138

l136: from l132
  swap w__[8,8,2] w__[8,9,0]
goto l137

l137: from l136
  skip
goto l133

l138: from l132
  skip
if (w__[6,9,0] || w__[8,9,0]) l140 l142

l140: from l138
  skip
if (w__[6,9,0]) l144 l146

l144: from l140
  swap w__[6,9,0] w__[9,9,1]
goto l145

l145: from l144
  skip
goto l141

l146: from l140
  swap w__[8,9,0] w__[9,9,1]
goto l147

l147: from l146
  skip
goto l141

l141: fi (1) l145 l147
  skip
goto l139

l142: from l138
  skip
if (w__[9,9,1]) l148 l150

l148: from l142
  skip
goto l152

l152: from l148
  skip
goto l153

l153: from l152
  swap w__[9,9,1] w__[9,9,2]
goto l149

l149: from l153
  skip
goto l143

l150: from l142
  skip
if (w__[9,9,2]) l154 l156

l154: from l150
  swap w__[9,9,2] w__[9,10,0]
goto l155

l155: from l154
  skip
goto l151

l156: from l150
  skip
if (w__[9,10,0]) l158 l160

l158: from l156
  swap w__[9,10,0] w__[10,10,1]
goto l159

l159: from l158
  skip
goto l157

l160: from l156
  skip
if (w__[10,10,1]) l162 l164

l162: from l160
  w_[0,1,0] ^= 1
goto l166

l166: from l162
  skip
goto l167

l167: from l166
  swap w__[10,10,1] w__[10,10,2]
goto l163

l163: from l167
  skip
goto l161

l164: from l160
  skip
if (w__[10,10,2]) l168 l170

l168: from l164
  swap w__[10,10,2] w__[10,11,0]
goto l169

l169: from l168
  skip
goto l165

l170: from l164
  skip
if (w__[10,11,0]) l172 l174

l172: from l170
  swap w__[10,11,0] w__[11,11,1]
goto l173

l173: from l172
  skip
goto l171

l174: from l170
  skip
if (w__[11,11,1]) l176 l178

l176: from l174
  skip
goto l180

l180: from l176
  skip
goto l181

l181: from l180
  swap w__[11,11,1] w__[11,11,2]
goto l177

l177: from l181
  skip
goto l175

l178: from l174
  skip
if (w__[11,11,2]) l182 l184

l182: from l178
  swap w__[11,11,2] w__[11,12,0]
goto l183

l183: from l182
  skip
goto l179

l184: from l178
  skip
if (w__[11,12,0] || w__[74,12,0]) l186 l188

l186: from l184
  skip
if (w__[11,12,0]) l190 l192

l190: from l186
  swap w__[11,12,0] w__[12,12,1]
goto l191

l191: from l190
  skip
goto l187

l192: from l186
  swap w__[74,12,0] w__[12,12,1]
goto l193

l193: from l192
  skip
goto l187

l187: fi (w_[0,1,0]) l191 l193
  skip
goto l185

l188: from l184
  skip
if (w__[12,12,1]) l194 l196

l194: from l188
  skip
goto l198

l198: from l194
  skip
goto l199

l199: from l198
  swap w__[12,12,1] w__[12,12,2]
goto l195

l195: from l199
  skip
goto l189

l196: from l188
  skip
if (w__[12,12,2]) l200 l202

l200: from l196
  swap w__[12,12,2] w__[12,13,0]
goto l201

l201: from l200
  skip
goto l197

l202: from l196
  skip
if (w__[12,13,0]) l204 l206

l204: from l202
  swap w__[12,13,0] w__[13,13,1]
goto l205

l205: from l204
  skip
goto l203

l206: from l202
  skip
if (w__[13,13,1]) l208 l210

l208: from l206
  skip
goto l212

l212: from l208
  skip
goto l213

l213: from l212
  swap w__[13,13,1] w__[13,13,2]
goto l209

l209: from l213
  skip
goto l207

l210: from l206
  skip
if (w__[13,13,2]) l214 l216

l214: from l210
  skip
if (w_[0,1,0]) l218 l220

l218: from l214
  swap w__[13,13,2] w__[13,14,0]
goto l219

l219: from l218
  skip
goto l215

l220: from l214
  swap w__[13,13,2] w__[13,16,0]
goto l221

l221: from l220
  skip
goto l215

l215: fi (w__[13,14,0]) l219 l221
  skip
goto l211

l216: from l210
  skip
if (w__[13,14,0]) l222 l224

l222: from l216
  swap w__[13,14,0] w__[14,14,1]
goto l223

l223: from l222
  skip
goto l217

l224: from l216
  skip
if (w__[14,14,1]) l226 l228

l226: from l224
  swap w_[0,1,0] w_[1,1,1]
goto l230

l230: from l226
  skip
goto l231

l231: from l230
  swap w__[14,14,1] w__[14,14,2]
goto l227

l227: from l231
  skip
goto l225

l228: from l224
  skip
if (w__[14,14,2]) l232 l234

l232: from l228
  swap w__[14,14,2] w__[14,15,0]
goto l233

l233: from l232
  skip
goto l229

l234: from l228
  skip
if (w__[14,15,0]) l236 l238

l236: from l234
  swap w__[14,15,0] w__[15,15,1]
goto l237

l237: from l236
  skip
goto l235

l238: from l234
  skip
if (w__[15,15,1]) l240 l242

l240: from l238
  skip
goto l244

l244: from l240
  skip
goto l245

l245: from l244
  swap w__[15,15,1] w__[15,15,2]
goto l241

l241: from l245
  skip
goto l239

l242: from l238
  skip
if (w__[15,15,2]) l246 l248

l246: from l242
  swap w__[15,15,2] w__[15,72,0]
goto l247

l247: from l246
  skip
goto l243

l248: from l242
  skip
if (w__[13,16,0]) l250 l252

l250: from l248
  swap w__[13,16,0] w__[16,16,1]
goto l251

l251: from l250
  skip
goto l249

l252: from l248
  skip
if (w__[16,16,1]) l254 l256

l254: from l252
  skip
goto l258

l258: from l254
  skip
goto l259

l259: from l258
  swap w__[16,16,1] w__[16,16,2]
goto l255

l255: from l259
  skip
goto l253

l256: from l252
  skip
if (w__[16,16,2]) l260 l262

l260: from l256
  skip
if (w_[1,1,1]) l264 l266

l264: from l260
  swap w__[16,16,2] w__[16,17,0]
goto l265

l265: from l264
  skip
goto l261

l266: from l260
  swap w__[16,16,2] w__[16,23,0]
goto l267

l267: from l266
  skip
goto l261

l261: fi (w__[16,17,0]) l265 l267
  skip
goto l257

l262: from l256
  skip
if (w__[16,17,0]) l268 l270

l268: from l262
  swap w__[16,17,0] w__[17,17,1]
goto l269

l269: from l268
  skip
goto l263

l270: from l262
  skip
if (w__[17,17,1]) l272 l274

l272: from l270
  n ^= 100
goto l276

l276: from l272
  skip
goto l277

l277: from l276
  swap w__[17,17,1] w__[17,17,2]
goto l273

l273: from l277
  skip
goto l271

l274: from l270
  skip
if (w__[17,17,2]) l278 l280

l278: from l274
  swap w__[17,17,2] w__[17,18,0]
goto l279

l279: from l278
  skip
goto l275

l280: from l274
  skip
if (w__[17,18,0]) l282 l284

l282: from l280
  swap w__[17,18,0] w__[18,18,1]
goto l283

l283: from l282
  skip
goto l281

l284: from l280
  skip
if (w__[18,18,1]) l286 l288

l286: from l284
  skip
goto l290

l290: from l286
  skip
goto l291

l291: from l290
  swap w__[18,18,1] w__[18,18,2]
goto l287

l287: from l291
  skip
goto l285

l288: from l284
  skip
if (w__[18,18,2]) l292 l294

l292: from l288
  swap w__[18,18,2] w__[18,19,0]
goto l293

l293: from l292
  skip
goto l289

l294: from l288
  skip
if (w__[18,19,0]) l296 l298

l296: from l294
  swap w__[18,19,0] w__[19,19,1]
goto l297

l297: from l296
  skip
goto l295

l298: from l294
  skip
if (w__[19,19,1]) l300 l302

l300: from l298
  w ^= 1
goto l304

l304: from l300
  skip
goto l305

l305: from l304
  swap w__[19,19,1] w__[19,19,2]
goto l301

l301: from l305
  skip
goto l299

l302: from l298
  skip
if (w__[19,19,2]) l306 l308

l306: from l302
  swap w__[19,19,2] w__[19,20,0]
goto l307

l307: from l306
  skip
goto l303

l308: from l302
  skip
if (w__[19,20,0]) l310 l312

l310: from l308
  swap w__[19,20,0] w__[20,20,1]
goto l311

l311: from l310
  skip
goto l309

l312: from l308
  skip
if (w__[20,20,1]) l314 l316

l314: from l312
  skip
goto l318

l318: from l314
  skip
goto l319

l319: from l318
  swap w__[20,20,1] w__[20,20,2]
goto l315

l315: from l319
  skip
goto l313

l316: from l312
  skip
if (w__[20,20,2]) l320 l322

l320: from l316
  swap w__[20,20,2] w__[20,21,0]
goto l321

l321: from l320
  skip
goto l317

l322: from l316
  skip
if (w__[20,21,0]) l324 l326

l324: from l322
  swap w__[20,21,0] w__[21,21,1]
goto l325

l325: from l324
  skip
goto l323

l326: from l322
  skip
if (w__[21,21,1]) l328 l330

l328: from l326
  swap w_[1,1,1] w_[1,1,2]
goto l332

l332: from l328
  skip
goto l333

l333: from l332
  swap w__[21,21,1] w__[21,21,2]
goto l329

l329: from l333
  skip
goto l327

l330: from l326
  skip
if (w__[21,21,2]) l334 l336

l334: from l330
  swap w__[21,21,2] w__[21,22,0]
goto l335

l335: from l334
  skip
goto l331

l336: from l330
  skip
if (w__[21,22,0]) l338 l340

l338: from l336
  swap w__[21,22,0] w__[22,22,1]
goto l339

l339: from l338
  skip
goto l337

l340: from l336
  skip
if (w__[22,22,1]) l342 l344

l342: from l340
  skip
goto l346

l346: from l342
  skip
goto l347

l347: from l346
  swap w__[22,22,1] w__[22,22,2]
goto l343

l343: from l347
  skip
goto l341

l344: from l340
  skip
if (w__[22,22,2]) l348 l350

l348: from l344
  swap w__[22,22,2] w__[22,71,0]
goto l349

l349: from l348
  skip
goto l345

l350: from l344
  skip
if (w__[16,23,0]) l352 l354

l352: from l350
  swap w__[16,23,0] w__[23,23,1]
goto l353

l353: from l352
  skip
goto l351

l354: from l350
  skip
if (w__[23,23,1]) l356 l358

l356: from l354
  skip
goto l360

l360: from l356
  skip
goto l361

l361: from l360
  swap w__[23,23,1] w__[23,23,2]
goto l357

l357: from l361
  skip
goto l355

l358: from l354
  skip
if (w__[23,23,2]) l362 l364

l362: from l358
  skip
if (w_[1,1,2]) l366 l368

l366: from l362
  swap w__[23,23,2] w__[23,24,0]
goto l367

l367: from l366
  skip
goto l363

l368: from l362
  swap w__[23,23,2] w__[23,26,0]
goto l369

l369: from l368
  skip
goto l363

l363: fi (w__[23,24,0]) l367 l369
  skip
goto l359

l364: from l358
  skip
if (w__[23,24,0]) l370 l372

l370: from l364
  swap w__[23,24,0] w__[24,24,1]
goto l371

l371: from l370
  skip
goto l365

l372: from l364
  skip
if (w__[24,24,1]) l374 l376

l374: from l372
  swap w_[1,1,2] w_[1,2,0]
goto l378

l378: from l374
  skip
goto l379

l379: from l378
  swap w__[24,24,1] w__[24,24,2]
goto l375

l375: from l379
  skip
goto l373

l376: from l372
  skip
if (w__[24,24,2]) l380 l382

l380: from l376
  swap w__[24,24,2] w__[24,25,0]
goto l381

l381: from l380
  skip
goto l377

l382: from l376
  skip
if (w__[24,25,0]) l384 l386

l384: from l382
  swap w__[24,25,0] w__[25,25,1]
goto l385

l385: from l384
  skip
goto l383

l386: from l382
  skip
if (w__[25,25,1]) l388 l390

l388: from l386
  skip
goto l392

l392: from l388
  skip
goto l393

l393: from l392
  swap w__[25,25,1] w__[25,25,2]
goto l389

l389: from l393
  skip
goto l387

l390: from l386
  skip
if (w__[25,25,2]) l394 l396

l394: from l390
  swap w__[25,25,2] w__[25,70,0]
goto l395

l395: from l394
  skip
goto l391

l396: from l390
  skip
if (w__[23,26,0]) l398 l400

l398: from l396
  swap w__[23,26,0] w__[26,26,1]
goto l399

l399: from l398
  skip
goto l397

l400: from l396
  skip
if (w__[26,26,1]) l402 l404

l402: from l400
  skip
goto l406

l406: from l402
  skip
goto l407

l407: from l406
  swap w__[26,26,1] w__[26,26,2]
goto l403

l403: from l407
  skip
goto l401

l404: from l400
  skip
if (w__[26,26,2]) l408 l410

l408: from l404
  skip
if (w_[1,2,0] || w_[2,2,0]) l412 l414

l412: from l408
  swap w__[26,26,2] w__[26,27,0]
goto l413

l413: from l412
  skip
goto l409

l414: from l408
  swap w__[26,26,2] w__[26,33,0]
goto l415

l415: from l414
  skip
goto l409

l409: fi (w__[26,27,0]) l413 l415
  skip
goto l405

l410: from l404
  skip
if (w__[26,27,0]) l416 l418

l416: from l410
  swap w__[26,27,0] w__[27,27,1]
goto l417

l417: from l416
  skip
goto l411

l418: from l410
  skip
if (w__[27,27,1]) l420 l422

l420: from l418
  skip
goto l424

l424: from l420
  skip
goto l425

l425: from l424
  swap w__[27,27,1] w__[27,27,2]
goto l421

l421: from l425
  skip
goto l419

l422: from l418
  skip
if (w__[27,27,2]) l426 l428

l426: from l422
  skip
if (w_[1,2,0]) l430 l432

l430: from l426
  swap w__[27,27,2] w__[27,28,0]
goto l431

l431: from l430
  skip
goto l427

l432: from l426
  swap w__[27,27,2] w__[27,30,0]
goto l433

l433: from l432
  skip
goto l427

l427: fi (w__[27,28,0]) l431 l433
  skip
goto l423

l428: from l422
  skip
if (w__[27,28,0]) l434 l436

l434: from l428
  swap w__[27,28,0] w__[28,28,1]
goto l435

l435: from l434
  skip
goto l429

l436: from l428
  skip
if (w__[28,28,1]) l438 l440

l438: from l436
  swap w_[1,2,0] w_[2,2,1]
goto l442

l442: from l438
  skip
goto l443

l443: from l442
  swap w__[28,28,1] w__[28,28,2]
goto l439

l439: from l443
  skip
goto l437

l440: from l436
  skip
if (w__[28,28,2]) l444 l446

l444: from l440
  swap w__[28,28,2] w__[28,29,0]
goto l445

l445: from l444
  skip
goto l441

l446: from l440
  skip
if (w__[28,29,0]) l448 l450

l448: from l446
  swap w__[28,29,0] w__[29,29,1]
goto l449

l449: from l448
  skip
goto l447

l450: from l446
  skip
if (w__[29,29,1]) l452 l454

l452: from l450
  skip
goto l456

l456: from l452
  skip
goto l457

l457: from l456
  swap w__[29,29,1] w__[29,29,2]
goto l453

l453: from l457
  skip
goto l451

l454: from l450
  skip
if (w__[29,29,2]) l458 l460

l458: from l454
  swap w__[29,29,2] w__[29,32,0]
goto l459

l459: from l458
  skip
goto l455

l460: from l454
  skip
if (w__[27,30,0]) l462 l464

l462: from l460
  swap w__[27,30,0] w__[30,30,1]
goto l463

l463: from l462
  skip
goto l461

l464: from l460
  skip
if (w__[30,30,1]) l466 l468

l466: from l464
  swap w_[2,2,0] w_[2,2,1]
goto l470

l470: from l466
  skip
goto l471

l471: from l470
  swap w__[30,30,1] w__[30,30,2]
goto l467

l467: from l471
  skip
goto l465

l468: from l464
  skip
if (w__[30,30,2]) l472 l474

l472: from l468
  swap w__[30,30,2] w__[30,31,0]
goto l473

l473: from l472
  skip
goto l469

l474: from l468
  skip
if (w__[30,31,0]) l476 l478

l476: from l474
  swap w__[30,31,0] w__[31,31,1]
goto l477

l477: from l476
  skip
goto l475

l478: from l474
  skip
if (w__[31,31,1]) l480 l482

l480: from l478
  skip
goto l484

l484: from l480
  skip
goto l485

l485: from l484
  swap w__[31,31,1] w__[31,31,2]
goto l481

l481: from l485
  skip
goto l479

l482: from l478
  skip
if (w__[31,31,2]) l486 l488

l486: from l482
  swap w__[31,31,2] w__[31,32,0]
goto l487

l487: from l486
  skip
goto l483

l488: from l482
  skip
if (w__[29,32,0] || w__[31,32,0]) l490 l492

l490: from l488
  skip
if (w__[29,32,0]) l494 l496

l494: from l490
  swap w__[29,32,0] w__[32,32,1]
goto l495

l495: from l494
  skip
goto l491

l496: from l490
  swap w__[31,32,0] w__[32,32,1]
goto l497

l497: from l496
  skip
goto l491

l491: fi (v = 0) l495 l497
  skip
goto l489

l492: from l488
  skip
if (w__[32,32,1]) l498 l500

l498: from l492
  skip
goto l502

l502: from l498
  skip
goto l503

l503: from l502
  swap w__[32,32,1] w__[32,32,2]
goto l499

l499: from l503
  skip
goto l493

l500: from l492
  skip
if (w__[32,32,2]) l504 l506

l504: from l500
  swap w__[32,32,2] w__[32,69,0]
goto l505

l505: from l504
  skip
goto l501

l506: from l500
  skip
if (w__[26,33,0]) l508 l510

l508: from l506
  swap w__[26,33,0] w__[33,33,1]
goto l509

l509: from l508
  skip
goto l507

l510: from l506
  skip
if (w__[33,33,1]) l512 l514

l512: from l510
  skip
goto l516

l516: from l512
  skip
goto l517

l517: from l516
  swap w__[33,33,1] w__[33,33,2]
goto l513

l513: from l517
  skip
goto l511

l514: from l510
  skip
if (w__[33,33,2]) l518 l520

l518: from l514
  skip
if (w_[2,2,1]) l522 l524

l522: from l518
  swap w__[33,33,2] w__[33,34,0]
goto l523

l523: from l522
  skip
goto l519

l524: from l518
  swap w__[33,33,2] w__[33,42,0]
goto l525

l525: from l524
  skip
goto l519

l519: fi (w__[33,34,0]) l523 l525
  skip
goto l515

l520: from l514
  skip
if (w__[33,34,0]) l526 l528

l526: from l520
  swap w__[33,34,0] w__[34,34,1]
goto l527

l527: from l526
  skip
goto l521

l528: from l520
  skip
if (w__[34,34,1]) l530 l532

l530: from l528
  v += w
goto l534

l534: from l530
  skip
goto l535

l535: from l534
  swap w__[34,34,1] w__[34,34,2]
goto l531

l531: from l535
  skip
goto l529

l532: from l528
  skip
if (w__[34,34,2]) l536 l538

l536: from l532
  swap w__[34,34,2] w__[34,35,0]
goto l537

l537: from l536
  skip
goto l533

l538: from l532
  skip
if (w__[34,35,0]) l540 l542

l540: from l538
  swap w__[34,35,0] w__[35,35,1]
goto l541

l541: from l540
  skip
goto l539

l542: from l538
  skip
if (w__[35,35,1]) l544 l546

l544: from l542
  skip
goto l548

l548: from l544
  skip
goto l549

l549: from l548
  swap w__[35,35,1] w__[35,35,2]
goto l545

l545: from l549
  skip
goto l543

l546: from l542
  skip
if (w__[35,35,2]) l550 l552

l550: from l546
  swap w__[35,35,2] w__[35,36,0]
goto l551

l551: from l550
  skip
goto l547

l552: from l546
  skip
if (w__[35,36,0]) l554 l556

l554: from l552
  swap w__[35,36,0] w__[36,36,1]
goto l555

l555: from l554
  skip
goto l553

l556: from l552
  skip
if (w__[36,36,1]) l558 l560

l558: from l556
  swap v w
goto l562

l562: from l558
  skip
goto l563

l563: from l562
  swap w__[36,36,1] w__[36,36,2]
goto l559

l559: from l563
  skip
goto l557

l560: from l556
  skip
if (w__[36,36,2]) l564 l566

l564: from l560
  swap w__[36,36,2] w__[36,37,0]
goto l565

l565: from l564
  skip
goto l561

l566: from l560
  skip
if (w__[36,37,0]) l568 l570

l568: from l566
  swap w__[36,37,0] w__[37,37,1]
goto l569

l569: from l568
  skip
goto l567

l570: from l566
  skip
if (w__[37,37,1]) l572 l574

l572: from l570
  skip
goto l576

l576: from l572
  skip
goto l577

l577: from l576
  swap w__[37,37,1] w__[37,37,2]
goto l573

l573: from l577
  skip
goto l571

l574: from l570
  skip
if (w__[37,37,2]) l578 l580

l578: from l574
  swap w__[37,37,2] w__[37,38,0]
goto l579

l579: from l578
  skip
goto l575

l580: from l574
  skip
if (w__[37,38,0]) l582 l584

l582: from l580
  swap w__[37,38,0] w__[38,38,1]
goto l583

l583: from l582
  skip
goto l581

l584: from l580
  skip
if (w__[38,38,1]) l586 l588

l586: from l584
  n -= 1
goto l590

l590: from l586
  skip
goto l591

l591: from l590
  swap w__[38,38,1] w__[38,38,2]
goto l587

l587: from l591
  skip
goto l585

l588: from l584
  skip
if (w__[38,38,2]) l592 l594

l592: from l588
  swap w__[38,38,2] w__[38,39,0]
goto l593

l593: from l592
  skip
goto l589

l594: from l588
  skip
if (w__[38,39,0]) l596 l598

l596: from l594
  swap w__[38,39,0] w__[39,39,1]
goto l597

l597: from l596
  skip
goto l595

l598: from l594
  skip
if (w__[39,39,1]) l600 l602

l600: from l598
  skip
goto l604

l604: from l600
  skip
goto l605

l605: from l604
  swap w__[39,39,1] w__[39,39,2]
goto l601

l601: from l605
  skip
goto l599

l602: from l598
  skip
if (w__[39,39,2]) l606 l608

l606: from l602
  swap w__[39,39,2] w__[39,40,0]
goto l607

l607: from l606
  skip
goto l603

l608: from l602
  skip
if (w__[39,40,0]) l610 l612

l610: from l608
  swap w__[39,40,0] w__[40,40,1]
goto l611

l611: from l610
  skip
goto l609

l612: from l608
  skip
if (w__[40,40,1]) l614 l616

l614: from l612
  swap w_[2,2,1] w_[2,2,2]
goto l618

l618: from l614
  skip
goto l619

l619: from l618
  swap w__[40,40,1] w__[40,40,2]
goto l615

l615: from l619
  skip
goto l613

l616: from l612
  skip
if (w__[40,40,2]) l620 l622

l620: from l616
  swap w__[40,40,2] w__[40,41,0]
goto l621

l621: from l620
  skip
goto l617

l622: from l616
  skip
if (w__[40,41,0]) l624 l626

l624: from l622
  swap w__[40,41,0] w__[41,41,1]
goto l625

l625: from l624
  skip
goto l623

l626: from l622
  skip
if (w__[41,41,1]) l628 l630

l628: from l626
  skip
goto l632

l632: from l628
  skip
goto l633

l633: from l632
  swap w__[41,41,1] w__[41,41,2]
goto l629

l629: from l633
  skip
goto l627

l630: from l626
  skip
if (w__[41,41,2]) l634 l636

l634: from l630
  swap w__[41,41,2] w__[41,68,0]
goto l635

l635: from l634
  skip
goto l631

l636: from l630
  skip
if (w__[33,42,0]) l638 l640

l638: from l636
  swap w__[33,42,0] w__[42,42,1]
goto l639

l639: from l638
  skip
goto l637

l640: from l636
  skip
if (w__[42,42,1]) l642 l644

l642: from l640
  skip
goto l646

l646: from l642
  skip
goto l647

l647: from l646
  swap w__[42,42,1] w__[42,42,2]
goto l643

l643: from l647
  skip
goto l641

l644: from l640
  skip
if (w__[42,42,2]) l648 l650

l648: from l644
  skip
if (w_[2,2,2]) l652 l654

l652: from l648
  swap w__[42,42,2] w__[42,43,0]
goto l653

l653: from l652
  skip
goto l649

l654: from l648
  swap w__[42,42,2] w__[42,49,0]
goto l655

l655: from l654
  skip
goto l649

l649: fi (w__[42,43,0]) l653 l655
  skip
goto l645

l650: from l644
  skip
if (w__[42,43,0]) l656 l658

l656: from l650
  swap w__[42,43,0] w__[43,43,1]
goto l657

l657: from l656
  skip
goto l651

l658: from l650
  skip
if (w__[43,43,1]) l660 l662

l660: from l658
  skip
goto l664

l664: from l660
  skip
goto l665

l665: from l664
  swap w__[43,43,1] w__[43,43,2]
goto l661

l661: from l665
  skip
goto l659

l662: from l658
  skip
if (w__[43,43,2]) l666 l668

l666: from l662
  skip
if (n = 0) l670 l672

l670: from l666
  swap w__[43,43,2] w__[43,44,0]
goto l671

l671: from l670
  skip
goto l667

l672: from l666
  swap w__[43,43,2] w__[43,46,0]
goto l673

l673: from l672
  skip
goto l667

l667: fi (w__[43,44,0]) l671 l673
  skip
goto l663

l668: from l662
  skip
if (w__[43,44,0]) l674 l676

l674: from l668
  swap w__[43,44,0] w__[44,44,1]
goto l675

l675: from l674
  skip
goto l669

l676: from l668
  skip
if (w__[44,44,1]) l678 l680

l678: from l676
  swap w_[2,2,2] w_[2,3,0]
goto l682

l682: from l678
  skip
goto l683

l683: from l682
  swap w__[44,44,1] w__[44,44,2]
goto l679

l679: from l683
  skip
goto l677

l680: from l676
  skip
if (w__[44,44,2]) l684 l686

l684: from l680
  swap w__[44,44,2] w__[44,45,0]
goto l685

l685: from l684
  skip
goto l681

l686: from l680
  skip
if (w__[44,45,0]) l688 l690

l688: from l686
  swap w__[44,45,0] w__[45,45,1]
goto l689

l689: from l688
  skip
goto l687

l690: from l686
  skip
if (w__[45,45,1]) l692 l694

l692: from l690
  skip
goto l696

l696: from l692
  skip
goto l697

l697: from l696
  swap w__[45,45,1] w__[45,45,2]
goto l693

l693: from l697
  skip
goto l691

l694: from l690
  skip
if (w__[45,45,2]) l698 l700

l698: from l694
  swap w__[45,45,2] w__[45,48,0]
goto l699

l699: from l698
  skip
goto l695

l700: from l694
  skip
if (w__[43,46,0]) l702 l704

l702: from l700
  swap w__[43,46,0] w__[46,46,1]
goto l703

l703: from l702
  skip
goto l701

l704: from l700
  skip
if (w__[46,46,1]) l706 l708

l706: from l704
  swap w_[2,2,2] w_[2,2,0]
goto l710

l710: from l706
  skip
goto l711

l711: from l710
  swap w__[46,46,1] w__[46,46,2]
goto l707

l707: from l711
  skip
goto l705

l708: from l704
  skip
if (w__[46,46,2]) l712 l714

l712: from l708
  swap w__[46,46,2] w__[46,47,0]
goto l713

l713: from l712
  skip
goto l709

l714: from l708
  skip
if (w__[46,47,0]) l716 l718

l716: from l714
  swap w__[46,47,0] w__[47,47,1]
goto l717

l717: from l716
  skip
goto l715

l718: from l714
  skip
if (w__[47,47,1]) l720 l722

l720: from l718
  skip
goto l724

l724: from l720
  skip
goto l725

l725: from l724
  swap w__[47,47,1] w__[47,47,2]
goto l721

l721: from l725
  skip
goto l719

l722: from l718
  skip
if (w__[47,47,2]) l726 l728

l726: from l722
  swap w__[47,47,2] w__[47,48,0]
goto l727

l727: from l726
  skip
goto l723

l728: from l722
  skip
if (w__[45,48,0] || w__[47,48,0]) l730 l732

l730: from l728
  skip
if (w__[45,48,0]) l734 l736

l734: from l730
  swap w__[45,48,0] w__[48,48,1]
goto l735

l735: from l734
  skip
goto l731

l736: from l730
  swap w__[47,48,0] w__[48,48,1]
goto l737

l737: from l736
  skip
goto l731

l731: fi (w_[2,3,0]) l735 l737
  skip
goto l729

l732: from l728
  skip
if (w__[48,48,1]) l738 l740

l738: from l732
  skip
goto l742

l742: from l738
  skip
goto l743

l743: from l742
  swap w__[48,48,1] w__[48,48,2]
goto l739

l739: from l743
  skip
goto l733

l740: from l732
  skip
if (w__[48,48,2]) l744 l746

l744: from l740
  swap w__[48,48,2] w__[48,67,0]
goto l745

l745: from l744
  skip
goto l741

l746: from l740
  skip
if (w__[42,49,0]) l748 l750

l748: from l746
  swap w__[42,49,0] w__[49,49,1]
goto l749

l749: from l748
  skip
goto l747

l750: from l746
  skip
if (w__[49,49,1]) l752 l754

l752: from l750
  skip
goto l756

l756: from l752
  skip
goto l757

l757: from l756
  swap w__[49,49,1] w__[49,49,2]
goto l753

l753: from l757
  skip
goto l751

l754: from l750
  skip
if (w__[49,49,2]) l758 l760

l758: from l754
  skip
if (w_[2,3,0]) l762 l764

l762: from l758
  swap w__[49,49,2] w__[49,50,0]
goto l763

l763: from l762
  skip
goto l759

l764: from l758
  swap w__[49,49,2] w__[49,52,0]
goto l765

l765: from l764
  skip
goto l759

l759: fi (w__[49,50,0]) l763 l765
  skip
goto l755

l760: from l754
  skip
if (w__[49,50,0]) l766 l768

l766: from l760
  swap w__[49,50,0] w__[50,50,1]
goto l767

l767: from l766
  skip
goto l761

l768: from l760
  skip
if (w__[50,50,1]) l770 l772

l770: from l768
  swap w_[2,3,0] w_[3,3,1]
goto l774

l774: from l770
  skip
goto l775

l775: from l774
  swap w__[50,50,1] w__[50,50,2]
goto l771

l771: from l775
  skip
goto l769

l772: from l768
  skip
if (w__[50,50,2]) l776 l778

l776: from l772
  swap w__[50,50,2] w__[50,51,0]
goto l777

l777: from l776
  skip
goto l773

l778: from l772
  skip
if (w__[50,51,0]) l780 l782

l780: from l778
  swap w__[50,51,0] w__[51,51,1]
goto l781

l781: from l780
  skip
goto l779

l782: from l778
  skip
if (w__[51,51,1]) l784 l786

l784: from l782
  skip
goto l788

l788: from l784
  skip
goto l789

l789: from l788
  swap w__[51,51,1] w__[51,51,2]
goto l785

l785: from l789
  skip
goto l783

l786: from l782
  skip
if (w__[51,51,2]) l790 l792

l790: from l786
  swap w__[51,51,2] w__[51,66,0]
goto l791

l791: from l790
  skip
goto l787

l792: from l786
  skip
if (w__[49,52,0]) l794 l796

l794: from l792
  swap w__[49,52,0] w__[52,52,1]
goto l795

l795: from l794
  skip
goto l793

l796: from l792
  skip
if (w__[52,52,1]) l798 l800

l798: from l796
  skip
goto l802

l802: from l798
  skip
goto l803

l803: from l802
  swap w__[52,52,1] w__[52,52,2]
goto l799

l799: from l803
  skip
goto l797

l800: from l796
  skip
if (w__[52,52,2]) l804 l806

l804: from l800
  skip
if (w_[3,3,1]) l808 l810

l808: from l804
  swap w__[52,52,2] w__[52,53,0]
goto l809

l809: from l808
  skip
goto l805

l810: from l804
  swap w__[52,52,2] w__[52,55,0]
goto l811

l811: from l810
  skip
goto l805

l805: fi (w__[52,53,0]) l809 l811
  skip
goto l801

l806: from l800
  skip
if (w__[52,53,0]) l812 l814

l812: from l806
  swap w__[52,53,0] w__[53,53,1]
goto l813

l813: from l812
  skip
goto l807

l814: from l806
  skip
if (w__[53,53,1]) l816 l818

l816: from l814
  swap w_[3,3,1] w_[3,3,2]
goto l820

l820: from l816
  skip
goto l821

l821: from l820
  swap w__[53,53,1] w__[53,53,2]
goto l817

l817: from l821
  skip
goto l815

l818: from l814
  skip
if (w__[53,53,2]) l822 l824

l822: from l818
  swap w__[53,53,2] w__[53,54,0]
goto l823

l823: from l822
  skip
goto l819

l824: from l818
  skip
if (w__[53,54,0]) l826 l828

l826: from l824
  swap w__[53,54,0] w__[54,54,1]
goto l827

l827: from l826
  skip
goto l825

l828: from l824
  skip
if (w__[54,54,1]) l830 l832

l830: from l828
  skip
goto l834

l834: from l830
  skip
goto l835

l835: from l834
  swap w__[54,54,1] w__[54,54,2]
goto l831

l831: from l835
  skip
goto l829

l832: from l828
  skip
if (w__[54,54,2]) l836 l838

l836: from l832
  swap w__[54,54,2] w__[54,65,0]
goto l837

l837: from l836
  skip
goto l833

l838: from l832
  skip
if (w__[52,55,0]) l840 l842

l840: from l838
  swap w__[52,55,0] w__[55,55,1]
goto l841

l841: from l840
  skip
goto l839

l842: from l838
  skip
if (w__[55,55,1]) l844 l846

l844: from l842
  skip
goto l848

l848: from l844
  skip
goto l849

l849: from l848
  swap w__[55,55,1] w__[55,55,2]
goto l845

l845: from l849
  skip
goto l843

l846: from l842
  skip
if (w__[55,55,2]) l850 l852

l850: from l846
  skip
if (w_[3,3,2]) l854 l856

l854: from l850
  swap w__[55,55,2] w__[55,56,0]
goto l855

l855: from l854
  skip
goto l851

l856: from l850
  swap w__[55,55,2] w__[55,58,0]
goto l857

l857: from l856
  skip
goto l851

l851: fi (w__[55,56,0]) l855 l857
  skip
goto l847

l852: from l846
  skip
if (w__[55,56,0]) l858 l860

l858: from l852
  swap w__[55,56,0] w__[56,56,1]
goto l859

l859: from l858
  skip
goto l853

l860: from l852
  skip
if (w__[56,56,1]) l862 l864

l862: from l860
  swap w_[3,3,2] w_[3,4,0]
goto l866

l866: from l862
  skip
goto l867

l867: from l866
  swap w__[56,56,1] w__[56,56,2]
goto l863

l863: from l867
  skip
goto l861

l864: from l860
  skip
if (w__[56,56,2]) l868 l870

l868: from l864
  swap w__[56,56,2] w__[56,57,0]
goto l869

l869: from l868
  skip
goto l865

l870: from l864
  skip
if (w__[56,57,0]) l872 l874

l872: from l870
  swap w__[56,57,0] w__[57,57,1]
goto l873

l873: from l872
  skip
goto l871

l874: from l870
  skip
if (w__[57,57,1]) l876 l878

l876: from l874
  skip
goto l880

l880: from l876
  skip
goto l881

l881: from l880
  swap w__[57,57,1] w__[57,57,2]
goto l877

l877: from l881
  skip
goto l875

l878: from l874
  skip
if (w__[57,57,2]) l882 l884

l882: from l878
  swap w__[57,57,2] w__[57,64,0]
goto l883

l883: from l882
  skip
goto l879

l884: from l878
  skip
if (w__[55,58,0]) l886 l888

l886: from l884
  swap w__[55,58,0] w__[58,58,1]
goto l887

l887: from l886
  skip
goto l885

l888: from l884
  skip
if (w__[58,58,1]) l890 l892

l890: from l888
  skip
goto l894

l894: from l890
  skip
goto l895

l895: from l894
  swap w__[58,58,1] w__[58,58,2]
goto l891

l891: from l895
  skip
goto l889

l892: from l888
  skip
if (w__[58,58,2]) l896 l898

l896: from l892
  skip
if (0) l900 l902

l900: from l896
  swap w__[58,58,2] w__[58,59,0]
goto l901

l901: from l900
  skip
goto l897

l902: from l896
  swap w__[58,58,2] w__[58,61,0]
goto l903

l903: from l902
  skip
goto l897

l897: fi (w__[58,59,0]) l901 l903
  skip
goto l893

l898: from l892
  skip
if (w__[58,59,0]) l904 l906

l904: from l898
  swap w__[58,59,0] w__[59,59,1]
goto l905

l905: from l904
  skip
goto l899

l906: from l898
  skip
if (w__[59,59,1]) l908 l910

l908: from l906
  skip
goto l912

l912: from l908
  skip
goto l913

l913: from l912
  swap w__[59,59,1] w__[59,59,2]
goto l909

l909: from l913
  skip
goto l907

l910: from l906
  skip
if (w__[59,59,2]) l914 l916

l914: from l910
  swap w__[59,59,2] w__[59,60,0]
goto l915

l915: from l914
  skip
goto l911

l916: from l910
  skip
if (w__[59,60,0]) l918 l920

l918: from l916
  swap w__[59,60,0] w__[60,60,1]
goto l919

l919: from l918
  skip
goto l917

l920: from l916
  skip
if (w__[60,60,1]) l922 l924

l922: from l920
  skip
goto l926

l926: from l922
  skip
goto l927

l927: from l926
  swap w__[60,60,1] w__[60,60,2]
goto l923

l923: from l927
  skip
goto l921

l924: from l920
  skip
if (w__[60,60,2]) l928 l930

l928: from l924
  swap w__[60,60,2] w__[60,63,0]
goto l929

l929: from l928
  skip
goto l925

l930: from l924
  skip
if (w__[58,61,0]) l932 l934

l932: from l930
  swap w__[58,61,0] w__[61,61,1]
goto l933

l933: from l932
  skip
goto l931

l934: from l930
  skip
if (w__[61,61,1]) l936 l938

l936: from l934
  skip
goto l940

l940: from l936
  skip
goto l941

l941: from l940
  swap w__[61,61,1] w__[61,61,2]
goto l937

l937: from l941
  skip
goto l935

l938: from l934
  skip
if (w__[61,61,2]) l942 l944

l942: from l938
  swap w__[61,61,2] w__[61,62,0]
goto l943

l943: from l942
  skip
goto l939

l944: from l938
  skip
if (w__[61,62,0]) l946 l948

l946: from l944
  swap w__[61,62,0] w__[62,62,1]
goto l947

l947: from l946
  skip
goto l945

l948: from l944
  skip
if (w__[62,62,1]) l950 l952

l950: from l948
  skip
goto l954

l954: from l950
  skip
goto l955

l955: from l954
  swap w__[62,62,1] w__[62,62,2]
goto l951

l951: from l955
  skip
goto l949

l952: from l948
  skip
if (w__[62,62,2]) l956 l958

l956: from l952
  swap w__[62,62,2] w__[62,63,0]
goto l957

l957: from l956
  skip
goto l953

l958: from l952
  skip
if (w__[60,63,0] || w__[62,63,0]) l960 l962

l960: from l958
  skip
if (w__[60,63,0]) l964 l966

l964: from l960
  swap w__[60,63,0] w__[63,63,1]
goto l965

l965: from l964
  skip
goto l961

l966: from l960
  swap w__[62,63,0] w__[63,63,1]
goto l967

l967: from l966
  skip
goto l961

l961: fi (1) l965 l967
  skip
goto l959

l962: from l958
  skip
if (w__[63,63,1]) l968 l970

l968: from l962
  skip
goto l972

l972: from l968
  skip
goto l973

l973: from l972
  swap w__[63,63,1] w__[63,63,2]
goto l969

l969: from l973
  skip
goto l963

l970: from l962
  skip
if (w__[63,63,2]) l974 l976

l974: from l970
  swap w__[63,63,2] w__[63,64,0]
goto l975

l975: from l974
  skip
goto l971

l976: from l970
  skip
if (w__[57,64,0] || w__[63,64,0]) l978 l980

l978: from l976
  skip
if (w__[57,64,0]) l982 l984

l982: from l978
  swap w__[57,64,0] w__[64,64,1]
goto l983

l983: from l982
  skip
goto l979

l984: from l978
  swap w__[63,64,0] w__[64,64,1]
goto l985

l985: from l984
  skip
goto l979

l979: fi (w_[3,4,0]) l983 l985
  skip
goto l977

l980: from l976
  skip
if (w__[64,64,1]) l986 l988

l986: from l980
  skip
goto l990

l990: from l986
  skip
goto l991

l991: from l990
  swap w__[64,64,1] w__[64,64,2]
goto l987

l987: from l991
  skip
goto l981

l988: from l980
  skip
if (w__[64,64,2]) l992 l994

l992: from l988
  swap w__[64,64,2] w__[64,65,0]
goto l993

l993: from l992
  skip
goto l989

l994: from l988
  skip
if (w__[54,65,0] || w__[64,65,0]) l996 l998

l996: from l994
  skip
if (w__[54,65,0]) l1000 l1002

l1000: from l996
  swap w__[54,65,0] w__[65,65,1]
goto l1001

l1001: from l1000
  skip
goto l997

l1002: from l996
  swap w__[64,65,0] w__[65,65,1]
goto l1003

l1003: from l1002
  skip
goto l997

l997: fi (w_[3,3,2]) l1001 l1003
  skip
goto l995

l998: from l994
  skip
if (w__[65,65,1]) l1004 l1006

l1004: from l998
  skip
goto l1008

l1008: from l1004
  skip
goto l1009

l1009: from l1008
  swap w__[65,65,1] w__[65,65,2]
goto l1005

l1005: from l1009
  skip
goto l999

l1006: from l998
  skip
if (w__[65,65,2]) l1010 l1012

l1010: from l1006
  swap w__[65,65,2] w__[65,66,0]
goto l1011

l1011: from l1010
  skip
goto l1007

l1012: from l1006
  skip
if (w__[51,66,0] || w__[65,66,0]) l1014 l1016

l1014: from l1012
  skip
if (w__[51,66,0]) l1018 l1020

l1018: from l1014
  swap w__[51,66,0] w__[66,66,1]
goto l1019

l1019: from l1018
  skip
goto l1015

l1020: from l1014
  swap w__[65,66,0] w__[66,66,1]
goto l1021

l1021: from l1020
  skip
goto l1015

l1015: fi (w_[3,3,1]) l1019 l1021
  skip
goto l1013

l1016: from l1012
  skip
if (w__[66,66,1]) l1022 l1024

l1022: from l1016
  skip
goto l1026

l1026: from l1022
  skip
goto l1027

l1027: from l1026
  swap w__[66,66,1] w__[66,66,2]
goto l1023

l1023: from l1027
  skip
goto l1017

l1024: from l1016
  skip
if (w__[66,66,2]) l1028 l1030

l1028: from l1024
  swap w__[66,66,2] w__[66,67,0]
goto l1029

l1029: from l1028
  skip
goto l1025

l1030: from l1024
  skip
if (w__[48,67,0] || w__[66,67,0]) l1032 l1034

l1032: from l1030
  skip
if (w__[48,67,0]) l1036 l1038

l1036: from l1032
  swap w__[48,67,0] w__[67,67,1]
goto l1037

l1037: from l1036
  skip
goto l1033

l1038: from l1032
  swap w__[66,67,0] w__[67,67,1]
goto l1039

l1039: from l1038
  skip
goto l1033

l1033: fi (w_[2,3,0] || w_[2,2,0]) l1037 l1039
  skip
goto l1031

l1034: from l1030
  skip
if (w__[67,67,1]) l1040 l1042

l1040: from l1034
  skip
goto l1044

l1044: from l1040
  skip
goto l1045

l1045: from l1044
  swap w__[67,67,1] w__[67,67,2]
goto l1041

l1041: from l1045
  skip
goto l1035

l1042: from l1034
  skip
if (w__[67,67,2]) l1046 l1048

l1046: from l1042
  swap w__[67,67,2] w__[67,68,0]
goto l1047

l1047: from l1046
  skip
goto l1043

l1048: from l1042
  skip
if (w__[41,68,0] || w__[67,68,0]) l1050 l1052

l1050: from l1048
  skip
if (w__[41,68,0]) l1054 l1056

l1054: from l1050
  swap w__[41,68,0] w__[68,68,1]
goto l1055

l1055: from l1054
  skip
goto l1051

l1056: from l1050
  swap w__[67,68,0] w__[68,68,1]
goto l1057

l1057: from l1056
  skip
goto l1051

l1051: fi (w_[2,2,2]) l1055 l1057
  skip
goto l1049

l1052: from l1048
  skip
if (w__[68,68,1]) l1058 l1060

l1058: from l1052
  skip
goto l1062

l1062: from l1058
  skip
goto l1063

l1063: from l1062
  swap w__[68,68,1] w__[68,68,2]
goto l1059

l1059: from l1063
  skip
goto l1053

l1060: from l1052
  skip
if (w__[68,68,2]) l1064 l1066

l1064: from l1060
  swap w__[68,68,2] w__[68,69,0]
goto l1065

l1065: from l1064
  skip
goto l1061

l1066: from l1060
  skip
if (w__[32,69,0] || w__[68,69,0]) l1068 l1070

l1068: from l1066
  skip
if (w__[32,69,0]) l1072 l1074

l1072: from l1068
  swap w__[32,69,0] w__[69,69,1]
goto l1073

l1073: from l1072
  skip
goto l1069

l1074: from l1068
  swap w__[68,69,0] w__[69,69,1]
goto l1075

l1075: from l1074
  skip
goto l1069

l1069: fi (w_[2,2,1]) l1073 l1075
  skip
goto l1067

l1070: from l1066
  skip
if (w__[69,69,1]) l1076 l1078

l1076: from l1070
  skip
goto l1080

l1080: from l1076
  skip
goto l1081

l1081: from l1080
  swap w__[69,69,1] w__[69,69,2]
goto l1077

l1077: from l1081
  skip
goto l1071

l1078: from l1070
  skip
if (w__[69,69,2]) l1082 l1084

l1082: from l1078
  swap w__[69,69,2] w__[69,70,0]
goto l1083

l1083: from l1082
  skip
goto l1079

l1084: from l1078
  skip
if (w__[25,70,0] || w__[69,70,0]) l1086 l1088

l1086: from l1084
  skip
if (w__[25,70,0]) l1090 l1092

l1090: from l1086
  swap w__[25,70,0] w__[70,70,1]
goto l1091

l1091: from l1090
  skip
goto l1087

l1092: from l1086
  swap w__[69,70,0] w__[70,70,1]
goto l1093

l1093: from l1092
  skip
goto l1087

l1087: fi (w_[1,2,0]) l1091 l1093
  skip
goto l1085

l1088: from l1084
  skip
if (w__[70,70,1]) l1094 l1096

l1094: from l1088
  skip
goto l1098

l1098: from l1094
  skip
goto l1099

l1099: from l1098
  swap w__[70,70,1] w__[70,70,2]
goto l1095

l1095: from l1099
  skip
goto l1089

l1096: from l1088
  skip
if (w__[70,70,2]) l1100 l1102

l1100: from l1096
  swap w__[70,70,2] w__[70,71,0]
goto l1101

l1101: from l1100
  skip
goto l1097

l1102: from l1096
  skip
if (w__[22,71,0] || w__[70,71,0]) l1104 l1106

l1104: from l1102
  skip
if (w__[22,71,0]) l1108 l1110

l1108: from l1104
  swap w__[22,71,0] w__[71,71,1]
goto l1109

l1109: from l1108
  skip
goto l1105

l1110: from l1104
  swap w__[70,71,0] w__[71,71,1]
goto l1111

l1111: from l1110
  skip
goto l1105

l1105: fi (w_[1,1,2]) l1109 l1111
  skip
goto l1103

l1106: from l1102
  skip
if (w__[71,71,1]) l1112 l1114

l1112: from l1106
  skip
goto l1116

l1116: from l1112
  skip
goto l1117

l1117: from l1116
  swap w__[71,71,1] w__[71,71,2]
goto l1113

l1113: from l1117
  skip
goto l1107

l1114: from l1106
  skip
if (w__[71,71,2]) l1118 l1120

l1118: from l1114
  swap w__[71,71,2] w__[71,72,0]
goto l1119

l1119: from l1118
  skip
goto l1115

l1120: from l1114
  skip
if (w__[15,72,0] || w__[71,72,0]) l1122 l1124

l1122: from l1120
  skip
if (w__[15,72,0]) l1126 l1128

l1126: from l1122
  swap w__[15,72,0] w__[72,72,1]
goto l1127

l1127: from l1126
  skip
goto l1123

l1128: from l1122
  swap w__[71,72,0] w__[72,72,1]
goto l1129

l1129: from l1128
  skip
goto l1123

l1123: fi (w_[1,1,1]) l1127 l1129
  skip
goto l1121

l1124: from l1120
  skip
if (w__[72,72,1]) l1130 l1132

l1130: from l1124
  skip
goto l1134

l1134: from l1130
  skip
goto l1135

l1135: from l1134
  swap w__[72,72,1] w__[72,72,2]
goto l1131

l1131: from l1135
  skip
goto l1125

l1132: from l1124
  skip
if (w__[72,72,2]) l1136 l1138

l1136: from l1132
  swap w__[72,72,2] w__[72,75,0]
goto l1137

l1137: from l1136
  skip
goto l1133

l1138: from l1132
  skip
if (w__[75,73,0]) l1140 l1142

l1140: from l1138
  swap w__[75,73,0] w__[73,73,1]
goto l1141

l1141: from l1140
  skip
goto l1139

l1142: from l1138
  skip
if (w__[73,73,1]) l1144 l1146

l1144: from l1142
  skip
goto l1148

l1148: from l1144
  skip
goto l1149

l1149: from l1148
  swap w__[73,73,1] w__[73,73,2]
goto l1145

l1145: from l1149
  skip
goto l1143

l1146: from l1142
  skip
if (w__[73,73,2]) l1150 l1152

l1150: from l1146
  swap w__[73,73,2] w__[73,74,0]
goto l1151

l1151: from l1150
  skip
goto l1147

l1152: from l1146
  skip
if (w__[73,74,0]) l1154 l1156

l1154: from l1152
  swap w__[73,74,0] w__[74,74,1]
goto l1155

l1155: from l1154
  skip
goto l1153

l1156: from l1152
  skip
if (w__[74,74,1]) l1158 l1160

l1158: from l1156
  skip
goto l1162

l1162: from l1158
  skip
goto l1163

l1163: from l1162
  swap w__[74,74,1] w__[74,74,2]
goto l1159

l1159: from l1163
  skip
goto l1157

l1160: from l1156
  skip
if (w__[74,74,2]) l1164 l1166

l1164: from l1160
  swap w__[74,74,2] w__[74,12,0]
goto l1165

l1165: from l1164
  skip
goto l1161

l1166: from l1160
  skip
if (w__[72,75,0]) l1168 l1170

l1168: from l1166
  swap w__[72,75,0] w__[75,75,1]
goto l1169

l1169: from l1168
  skip
goto l1167

l1170: from l1166
  skip
if (w__[75,75,1]) l1172 l1174

l1172: from l1170
  skip
goto l1176

l1176: from l1172
  skip
goto l1177

l1177: from l1176
  swap w__[75,75,1] w__[75,75,2]
goto l1173

l1173: from l1177
  skip
goto l1171

l1174: from l1170
  skip
if (w__[75,75,2]) l1178 l1180

l1178: from l1174
  skip
if (w_[3,4,0]) l1182 l1184

l1182: from l1178
  swap w__[75,75,2] w__[75,76,0]
goto l1183

l1183: from l1182
  skip
goto l1179

l1184: from l1178
  swap w__[75,75,2] w__[75,73,0]
goto l1185

l1185: from l1184
  skip
goto l1179

l1179: fi (w__[75,76,0]) l1183 l1185
  skip
goto l1175

l1180: from l1174
  skip
if (w__[75,76,0]) l1186 l1188

l1186: from l1180
  swap w__[75,76,0] w__[76,76,1]
goto l1187

l1187: from l1186
  skip
goto l1181

l1188: from l1180
  skip
if (w__[76,76,1]) l1190 l1192

l1190: from l1188
  w_[3,4,0] ^= 1
goto l1194

l1194: from l1190
  skip
goto l1195

l1195: from l1194
  swap w__[76,76,1] w__[76,76,2]
goto l1191

l1191: from l1195
  skip
goto l1189

l1192: from l1188
  skip
if (w__[76,76,2]) l1196 l1198

l1196: from l1192
  swap w__[76,76,2] w__[76,77,0]
goto l1197

l1197: from l1196
  skip
goto l1193

l1198: from l1192
  skip
if (w__[76,77,0]) l1200 l1202

l1200: from l1198
  swap w__[76,77,0] w__[77,77,1]
goto l1201

l1201: from l1200
  skip
goto l1199

l1202: from l1198
  skip
if (w__[77,77,1]) l1204 l1206

l1204: from l1202
  skip
goto l1208

l1208: from l1204
  skip
goto l1209

l1209: from l1208
  swap w__[77,77,1] w__[77,77,2]
goto l1205

l1205: from l1209
  skip
goto l1203

l1206: from l1202
  skip
if (w__[77,77,2]) l1210 l1212

l1210: from l1206
  swap w__[77,77,2] w__[77,78,0]
goto l1211

l1211: from l1210
  skip
goto l1207

l1212: from l1206
  skip
if (w__[77,78,0]) l1214 l1216

l1214: from l1212
  swap w__[77,78,0] w__[78,78,1]
goto l1215

l1215: from l1214
  skip
goto l1213

l1216: from l1212
  skip
if (w__[78,78,1]) l1218 l1220

l1218: from l1216
  skip
goto l1222

l1222: from l1218
  skip
goto l1223

l1223: from l1222
  swap w__[78,78,1] w__[78,78,2]
goto l1219

l1219: from l1223
  skip
goto l1217

l1220: from l1216
  skip
if (w__[78,78,2]) l1224 l1226

l1224: from l1220
  skip
if (null w_) l1228 l1230

l1228: from l1224
  swap w__[78,78,2] w__[78,79,0]
goto l1229

l1229: from l1228
  skip
goto l1225

l1230: from l1224
  swap w__[78,78,2] w__[78,81,0]
goto l1231

l1231: from l1230
  skip
goto l1225

l1225: fi (w__[78,79,0]) l1229 l1231
  skip
goto l1221

l1226: from l1220
  skip
if (w__[78,79,0]) l1232 l1234

l1232: from l1226
  swap w__[78,79,0] w__[79,79,1]
goto l1233

l1233: from l1232
  skip
goto l1227

l1234: from l1226
  skip
if (w__[79,79,1]) l1236 l1238

l1236: from l1234
  skip
goto l1240

l1240: from l1236
  skip
goto l1241

l1241: from l1240
  swap w__[79,79,1] w__[79,79,2]
goto l1237

l1237: from l1241
  skip
goto l1235

l1238: from l1234
  skip
if (w__[79,79,2]) l1242 l1244

l1242: from l1238
  swap w__[79,79,2] w__[79,80,0]
goto l1243

l1243: from l1242
  skip
goto l1239

l1244: from l1238
  skip
if (w__[79,80,0]) l1246 l1248

l1246: from l1244
  swap w__[79,80,0] w__[80,80,1]
goto l1247

l1247: from l1246
  skip
goto l1245

l1248: from l1244
  skip
if (w__[80,80,1]) l1250 l1252

l1250: from l1248
  skip
goto l1254

l1254: from l1250
  skip
goto l1255

l1255: from l1254
  swap w__[80,80,1] w__[80,80,2]
goto l1251

l1251: from l1255
  skip
goto l1249

l1252: from l1248
  skip
if (w__[80,80,2]) l1256 l1258

l1256: from l1252
  swap w__[80,80,2] w__[80,83,0]
goto l1257

l1257: from l1256
  skip
goto l1253

l1258: from l1252
  skip
if (w__[78,81,0]) l1260 l1262

l1260: from l1258
  swap w__[78,81,0] w__[81,81,1]
goto l1261

l1261: from l1260
  skip
goto l1259

l1262: from l1258
  skip
if (w__[81,81,1]) l1264 l1266

l1264: from l1262
  skip
goto l1268

l1268: from l1264
  skip
goto l1269

l1269: from l1268
  swap w__[81,81,1] w__[81,81,2]
goto l1265

l1265: from l1269
  skip
goto l1263

l1266: from l1262
  skip
if (w__[81,81,2]) l1270 l1272

l1270: from l1266
  swap w__[81,81,2] w__[81,82,0]
goto l1271

l1271: from l1270
  skip
goto l1267

l1272: from l1266
  skip
if (w__[81,82,0]) l1274 l1276

l1274: from l1272
  swap w__[81,82,0] w__[82,82,1]
goto l1275

l1275: from l1274
  skip
goto l1273

l1276: from l1272
  skip
if (w__[82,82,1]) l1278 l1280

l1278: from l1276
  skip
goto l1282

l1282: from l1278
  skip
goto l1283

l1283: from l1282
  swap w__[82,82,1] w__[82,82,2]
goto l1279

l1279: from l1283
  skip
goto l1277

l1280: from l1276
  skip
if (w__[82,82,2]) l1284 l1286

l1284: from l1280
  swap w__[82,82,2] w__[82,83,0]
goto l1285

l1285: from l1284
  skip
goto l1281

l1286: from l1280
  skip
if (w__[80,83,0] || w__[82,83,0]) l1288 l1290

l1288: from l1286
  skip
if (w__[80,83,0]) l1292 l1294

l1292: from l1288
  swap w__[80,83,0] w__[83,83,1]
goto l1293

l1293: from l1292
  skip
goto l1289

l1294: from l1288
  swap w__[82,83,0] w__[83,83,1]
goto l1295

l1295: from l1294
  skip
goto l1289

l1289: fi (1) l1293 l1295
  skip
goto l1287

l1290: from l1286
  skip
if (w__[83,83,1]) l1296 l1298

l1296: from l1290
  skip
goto l1300

l1300: from l1296
  skip
goto l1301

l1301: from l1300
  swap w__[83,83,1] w__[83,83,2]
goto l1297

l1297: from l1301
  skip
goto l1291

l1298: from l1290
  skip
if (w__[83,83,2]) l1302 l1304

l1302: from l1298
  swap w__[83,83,2] w__[83,84,0]
goto l1303

l1303: from l1302
  skip
goto l1299

l1304: from l1298
  skip
if (w__[83,84,0]) l1306 l1308

l1306: from l1304
  swap w__[83,84,0] w__[84,84,1]
goto l1307

l1307: from l1306
  skip
goto l1305

l1308: from l1304
  skip
if (w__[84,84,1]) l1310 l1312

l1310: from l1308
  free w_ [5,5,3]
goto l1314

l1314: from l1310
  skip
goto l1315

l1315: from l1314
  swap w__[84,84,1] w__[84,84,2]
goto l1311

l1311: from l1315
  skip
goto l1309

l1312: from l1308
  skip
if (w__[84,84,2]) l1316 l1318

l1316: from l1312
  swap w__[84,84,2] w__[84,85,0]
goto l1317

l1317: from l1316
  skip
goto l1313

l1318: from l1312
  skip
if (w__[84,85,0]) l1320 l1322

l1320: from l1318
  swap w__[84,85,0] w__[85,85,1]
goto l1321

l1321: from l1320
  skip
goto l1319

l1322: from l1318
  skip
if (w__[85,85,1]) l1324 l1326

l1324: from l1322
  skip
goto l1328

l1328: from l1324
  skip
goto l1329

l1329: from l1328
  swap w__[85,85,1] w__[85,85,2]
goto l1325

l1325: from l1329
  skip
goto l1323

l1326: from l1322
  skip
if (w__[85,85,2]) l1330 l1332

l1330: from l1326
  swap w__[85,85,2] w__[85,86,0]
goto l1331

l1331: from l1330
  skip
goto l1327

l1332: from l1326
  skip
if (w__[85,86,0]) l1334 l1336

l1334: from l1332
  swap w__[85,86,0] w__[86,86,1]
goto l1335

l1335: from l1334
  skip
goto l1333

l1336: from l1332
  skip
if (w__[86,86,1]) l1338 l1340

l1338: from l1336
  skip
goto l1342

l1342: from l1338
  skip
goto l1343

l1343: from l1342
  swap w__[86,86,1] w__[86,86,2]
goto l1339

l1339: from l1343
  skip
goto l1337

l1340: from l1336
  skip
if (w__[86,86,2]) l1344 l1346

l1344: from l1340
  swap w__[86,86,2] w__[86,87,0]
goto l1345

l1345: from l1344
  skip
goto l1341

l1346: from l1340
  skip
if (0) l1348 l1350

l1348: from l1346
  skip
goto l1349

l1349: from l1348
  skip
goto l1347

l1350: from l1346
  skip
goto l1351

l1351: from l1350
  skip
goto l1347

l1347: fi (1) l1349 l1351
  skip
goto l1341

l1341: fi (w__[86,87,0]) l1345 l1347
  skip
goto l1337

l1337: fi (w__[86,86,2]) l1339 l1341
  skip
goto l1333

l1333: fi (w__[86,86,1]) l1335 l1337
  skip
goto l1327

l1327: fi (w__[85,86,0]) l1331 l1333
  skip
goto l1323

l1323: fi (w__[85,85,2]) l1325 l1327
  skip
goto l1319

l1319: fi (w__[85,85,1]) l1321 l1323
  skip
goto l1313

l1313: fi (w__[84,85,0]) l1317 l1319
  skip
goto l1309

l1309: fi (w__[84,84,2]) l1311 l1313
  skip
goto l1305

l1305: fi (w__[84,84,1]) l1307 l1309
  skip
goto l1299

l1299: fi (w__[83,84,0]) l1303 l1305
  skip
goto l1291

l1291: fi (w__[83,83,2]) l1297 l1299
  skip
goto l1287

l1287: fi (w__[83,83,1]) l1289 l1291
  skip
goto l1281

l1281: fi (w__[82,83,0]) l1285 l1287
  skip
goto l1277

l1277: fi (w__[82,82,2]) l1279 l1281
  skip
goto l1273

l1273: fi (w__[82,82,1]) l1275 l1277
  skip
goto l1267

l1267: fi (w__[81,82,0]) l1271 l1273
  skip
goto l1263

l1263: fi (w__[81,81,2]) l1265 l1267
  skip
goto l1259

l1259: fi (w__[81,81,1]) l1261 l1263
  skip
goto l1253

l1253: fi (w__[80,83,0]) l1257 l1259
  skip
goto l1249

l1249: fi (w__[80,80,2]) l1251 l1253
  skip
goto l1245

l1245: fi (w__[80,80,1]) l1247 l1249
  skip
goto l1239

l1239: fi (w__[79,80,0]) l1243 l1245
  skip
goto l1235

l1235: fi (w__[79,79,2]) l1237 l1239
  skip
goto l1227

l1227: fi (w__[79,79,1]) l1233 l1235
  skip
goto l1221

l1221: fi (w__[78,79,0] || w__[78,81,0]) l1225 l1227
  skip
goto l1217

l1217: fi (w__[78,78,2]) l1219 l1221
  skip
goto l1213

l1213: fi (w__[78,78,1]) l1215 l1217
  skip
goto l1207

l1207: fi (w__[77,78,0]) l1211 l1213
  skip
goto l1203

l1203: fi (w__[77,77,2]) l1205 l1207
  skip
goto l1199

l1199: fi (w__[77,77,1]) l1201 l1203
  skip
goto l1193

l1193: fi (w__[76,77,0]) l1197 l1199
  skip
goto l1189

l1189: fi (w__[76,76,2]) l1191 l1193
  skip
goto l1181

l1181: fi (w__[76,76,1]) l1187 l1189
  skip
goto l1175

l1175: fi (w__[75,76,0] || w__[75,73,0]) l1179 l1181
  skip
goto l1171

l1171: fi (w__[75,75,2]) l1173 l1175
  skip
goto l1167

l1167: fi (w__[75,75,1]) l1169 l1171
  skip
goto l1161

l1161: fi (w__[74,12,0]) l1165 l1167
  skip
goto l1157

l1157: fi (w__[74,74,2]) l1159 l1161
  skip
goto l1153

l1153: fi (w__[74,74,1]) l1155 l1157
  skip
goto l1147

l1147: fi (w__[73,74,0]) l1151 l1153
  skip
goto l1143

l1143: fi (w__[73,73,2]) l1145 l1147
  skip
goto l1139

l1139: fi (w__[73,73,1]) l1141 l1143
  skip
goto l1133

l1133: fi (w__[72,75,0]) l1137 l1139
  skip
goto l1125

l1125: fi (w__[72,72,2]) l1131 l1133
  skip
goto l1121

l1121: fi (w__[72,72,1]) l1123 l1125
  skip
goto l1115

l1115: fi (w__[71,72,0]) l1119 l1121
  skip
goto l1107

l1107: fi (w__[71,71,2]) l1113 l1115
  skip
goto l1103

l1103: fi (w__[71,71,1]) l1105 l1107
  skip
goto l1097

l1097: fi (w__[70,71,0]) l1101 l1103
  skip
goto l1089

l1089: fi (w__[70,70,2]) l1095 l1097
  skip
goto l1085

l1085: fi (w__[70,70,1]) l1087 l1089
  skip
goto l1079

l1079: fi (w__[69,70,0]) l1083 l1085
  skip
goto l1071

l1071: fi (w__[69,69,2]) l1077 l1079
  skip
goto l1067

l1067: fi (w__[69,69,1]) l1069 l1071
  skip
goto l1061

l1061: fi (w__[68,69,0]) l1065 l1067
  skip
goto l1053

l1053: fi (w__[68,68,2]) l1059 l1061
  skip
goto l1049

l1049: fi (w__[68,68,1]) l1051 l1053
  skip
goto l1043

l1043: fi (w__[67,68,0]) l1047 l1049
  skip
goto l1035

l1035: fi (w__[67,67,2]) l1041 l1043
  skip
goto l1031

l1031: fi (w__[67,67,1]) l1033 l1035
  skip
goto l1025

l1025: fi (w__[66,67,0]) l1029 l1031
  skip
goto l1017

l1017: fi (w__[66,66,2]) l1023 l1025
  skip
goto l1013

l1013: fi (w__[66,66,1]) l1015 l1017
  skip
goto l1007

l1007: fi (w__[65,66,0]) l1011 l1013
  skip
goto l999

l999: fi (w__[65,65,2]) l1005 l1007
  skip
goto l995

l995: fi (w__[65,65,1]) l997 l999
  skip
goto l989

l989: fi (w__[64,65,0]) l993 l995
  skip
goto l981

l981: fi (w__[64,64,2]) l987 l989
  skip
goto l977

l977: fi (w__[64,64,1]) l979 l981
  skip
goto l971

l971: fi (w__[63,64,0]) l975 l977
  skip
goto l963

l963: fi (w__[63,63,2]) l969 l971
  skip
goto l959

l959: fi (w__[63,63,1]) l961 l963
  skip
goto l953

l953: fi (w__[62,63,0]) l957 l959
  skip
goto l949

l949: fi (w__[62,62,2]) l951 l953
  skip
goto l945

l945: fi (w__[62,62,1]) l947 l949
  skip
goto l939

l939: fi (w__[61,62,0]) l943 l945
  skip
goto l935

l935: fi (w__[61,61,2]) l937 l939
  skip
goto l931

l931: fi (w__[61,61,1]) l933 l935
  skip
goto l925

l925: fi (w__[60,63,0]) l929 l931
  skip
goto l921

l921: fi (w__[60,60,2]) l923 l925
  skip
goto l917

l917: fi (w__[60,60,1]) l919 l921
  skip
goto l911

l911: fi (w__[59,60,0]) l915 l917
  skip
goto l907

l907: fi (w__[59,59,2]) l909 l911
  skip
goto l899

l899: fi (w__[59,59,1]) l905 l907
  skip
goto l893

l893: fi (w__[58,59,0] || w__[58,61,0]) l897 l899
  skip
goto l889

l889: fi (w__[58,58,2]) l891 l893
  skip
goto l885

l885: fi (w__[58,58,1]) l887 l889
  skip
goto l879

l879: fi (w__[57,64,0]) l883 l885
  skip
goto l875

l875: fi (w__[57,57,2]) l877 l879
  skip
goto l871

l871: fi (w__[57,57,1]) l873 l875
  skip
goto l865

l865: fi (w__[56,57,0]) l869 l871
  skip
goto l861

l861: fi (w__[56,56,2]) l863 l865
  skip
goto l853

l853: fi (w__[56,56,1]) l859 l861
  skip
goto l847

l847: fi (w__[55,56,0] || w__[55,58,0]) l851 l853
  skip
goto l843

l843: fi (w__[55,55,2]) l845 l847
  skip
goto l839

l839: fi (w__[55,55,1]) l841 l843
  skip
goto l833

l833: fi (w__[54,65,0]) l837 l839
  skip
goto l829

l829: fi (w__[54,54,2]) l831 l833
  skip
goto l825

l825: fi (w__[54,54,1]) l827 l829
  skip
goto l819

l819: fi (w__[53,54,0]) l823 l825
  skip
goto l815

l815: fi (w__[53,53,2]) l817 l819
  skip
goto l807

l807: fi (w__[53,53,1]) l813 l815
  skip
goto l801

l801: fi (w__[52,53,0] || w__[52,55,0]) l805 l807
  skip
goto l797

l797: fi (w__[52,52,2]) l799 l801
  skip
goto l793

l793: fi (w__[52,52,1]) l795 l797
  skip
goto l787

l787: fi (w__[51,66,0]) l791 l793
  skip
goto l783

l783: fi (w__[51,51,2]) l785 l787
  skip
goto l779

l779: fi (w__[51,51,1]) l781 l783
  skip
goto l773

l773: fi (w__[50,51,0]) l777 l779
  skip
goto l769

l769: fi (w__[50,50,2]) l771 l773
  skip
goto l761

l761: fi (w__[50,50,1]) l767 l769
  skip
goto l755

l755: fi (w__[49,50,0] || w__[49,52,0]) l759 l761
  skip
goto l751

l751: fi (w__[49,49,2]) l753 l755
  skip
goto l747

l747: fi (w__[49,49,1]) l749 l751
  skip
goto l741

l741: fi (w__[48,67,0]) l745 l747
  skip
goto l733

l733: fi (w__[48,48,2]) l739 l741
  skip
goto l729

l729: fi (w__[48,48,1]) l731 l733
  skip
goto l723

l723: fi (w__[47,48,0]) l727 l729
  skip
goto l719

l719: fi (w__[47,47,2]) l721 l723
  skip
goto l715

l715: fi (w__[47,47,1]) l717 l719
  skip
goto l709

l709: fi (w__[46,47,0]) l713 l715
  skip
goto l705

l705: fi (w__[46,46,2]) l707 l709
  skip
goto l701

l701: fi (w__[46,46,1]) l703 l705
  skip
goto l695

l695: fi (w__[45,48,0]) l699 l701
  skip
goto l691

l691: fi (w__[45,45,2]) l693 l695
  skip
goto l687

l687: fi (w__[45,45,1]) l689 l691
  skip
goto l681

l681: fi (w__[44,45,0]) l685 l687
  skip
goto l677

l677: fi (w__[44,44,2]) l679 l681
  skip
goto l669

l669: fi (w__[44,44,1]) l675 l677
  skip
goto l663

l663: fi (w__[43,44,0] || w__[43,46,0]) l667 l669
  skip
goto l659

l659: fi (w__[43,43,2]) l661 l663
  skip
goto l651

l651: fi (w__[43,43,1]) l657 l659
  skip
goto l645

l645: fi (w__[42,43,0] || w__[42,49,0]) l649 l651
  skip
goto l641

l641: fi (w__[42,42,2]) l643 l645
  skip
goto l637

l637: fi (w__[42,42,1]) l639 l641
  skip
goto l631

l631: fi (w__[41,68,0]) l635 l637
  skip
goto l627

l627: fi (w__[41,41,2]) l629 l631
  skip
goto l623

l623: fi (w__[41,41,1]) l625 l627
  skip
goto l617

l617: fi (w__[40,41,0]) l621 l623
  skip
goto l613

l613: fi (w__[40,40,2]) l615 l617
  skip
goto l609

l609: fi (w__[40,40,1]) l611 l613
  skip
goto l603

l603: fi (w__[39,40,0]) l607 l609
  skip
goto l599

l599: fi (w__[39,39,2]) l601 l603
  skip
goto l595

l595: fi (w__[39,39,1]) l597 l599
  skip
goto l589

l589: fi (w__[38,39,0]) l593 l595
  skip
goto l585

l585: fi (w__[38,38,2]) l587 l589
  skip
goto l581

l581: fi (w__[38,38,1]) l583 l585
  skip
goto l575

l575: fi (w__[37,38,0]) l579 l581
  skip
goto l571

l571: fi (w__[37,37,2]) l573 l575
  skip
goto l567

l567: fi (w__[37,37,1]) l569 l571
  skip
goto l561

l561: fi (w__[36,37,0]) l565 l567
  skip
goto l557

l557: fi (w__[36,36,2]) l559 l561
  skip
goto l553

l553: fi (w__[36,36,1]) l555 l557
  skip
goto l547

l547: fi (w__[35,36,0]) l551 l553
  skip
goto l543

l543: fi (w__[35,35,2]) l545 l547
  skip
goto l539

l539: fi (w__[35,35,1]) l541 l543
  skip
goto l533

l533: fi (w__[34,35,0]) l537 l539
  skip
goto l529

l529: fi (w__[34,34,2]) l531 l533
  skip
goto l521

l521: fi (w__[34,34,1]) l527 l529
  skip
goto l515

l515: fi (w__[33,34,0] || w__[33,42,0]) l519 l521
  skip
goto l511

l511: fi (w__[33,33,2]) l513 l515
  skip
goto l507

l507: fi (w__[33,33,1]) l509 l511
  skip
goto l501

l501: fi (w__[32,69,0]) l505 l507
  skip
goto l493

l493: fi (w__[32,32,2]) l499 l501
  skip
goto l489

l489: fi (w__[32,32,1]) l491 l493
  skip
goto l483

l483: fi (w__[31,32,0]) l487 l489
  skip
goto l479

l479: fi (w__[31,31,2]) l481 l483
  skip
goto l475

l475: fi (w__[31,31,1]) l477 l479
  skip
goto l469

l469: fi (w__[30,31,0]) l473 l475
  skip
goto l465

l465: fi (w__[30,30,2]) l467 l469
  skip
goto l461

l461: fi (w__[30,30,1]) l463 l465
  skip
goto l455

l455: fi (w__[29,32,0]) l459 l461
  skip
goto l451

l451: fi (w__[29,29,2]) l453 l455
  skip
goto l447

l447: fi (w__[29,29,1]) l449 l451
  skip
goto l441

l441: fi (w__[28,29,0]) l445 l447
  skip
goto l437

l437: fi (w__[28,28,2]) l439 l441
  skip
goto l429

l429: fi (w__[28,28,1]) l435 l437
  skip
goto l423

l423: fi (w__[27,28,0] || w__[27,30,0]) l427 l429
  skip
goto l419

l419: fi (w__[27,27,2]) l421 l423
  skip
goto l411

l411: fi (w__[27,27,1]) l417 l419
  skip
goto l405

l405: fi (w__[26,27,0] || w__[26,33,0]) l409 l411
  skip
goto l401

l401: fi (w__[26,26,2]) l403 l405
  skip
goto l397

l397: fi (w__[26,26,1]) l399 l401
  skip
goto l391

l391: fi (w__[25,70,0]) l395 l397
  skip
goto l387

l387: fi (w__[25,25,2]) l389 l391
  skip
goto l383

l383: fi (w__[25,25,1]) l385 l387
  skip
goto l377

l377: fi (w__[24,25,0]) l381 l383
  skip
goto l373

l373: fi (w__[24,24,2]) l375 l377
  skip
goto l365

l365: fi (w__[24,24,1]) l371 l373
  skip
goto l359

l359: fi (w__[23,24,0] || w__[23,26,0]) l363 l365
  skip
goto l355

l355: fi (w__[23,23,2]) l357 l359
  skip
goto l351

l351: fi (w__[23,23,1]) l353 l355
  skip
goto l345

l345: fi (w__[22,71,0]) l349 l351
  skip
goto l341

l341: fi (w__[22,22,2]) l343 l345
  skip
goto l337

l337: fi (w__[22,22,1]) l339 l341
  skip
goto l331

l331: fi (w__[21,22,0]) l335 l337
  skip
goto l327

l327: fi (w__[21,21,2]) l329 l331
  skip
goto l323

l323: fi (w__[21,21,1]) l325 l327
  skip
goto l317

l317: fi (w__[20,21,0]) l321 l323
  skip
goto l313

l313: fi (w__[20,20,2]) l315 l317
  skip
goto l309

l309: fi (w__[20,20,1]) l311 l313
  skip
goto l303

l303: fi (w__[19,20,0]) l307 l309
  skip
goto l299

l299: fi (w__[19,19,2]) l301 l303
  skip
goto l295

l295: fi (w__[19,19,1]) l297 l299
  skip
goto l289

l289: fi (w__[18,19,0]) l293 l295
  skip
goto l285

l285: fi (w__[18,18,2]) l287 l289
  skip
goto l281

l281: fi (w__[18,18,1]) l283 l285
  skip
goto l275

l275: fi (w__[17,18,0]) l279 l281
  skip
goto l271

l271: fi (w__[17,17,2]) l273 l275
  skip
goto l263

l263: fi (w__[17,17,1]) l269 l271
  skip
goto l257

l257: fi (w__[16,17,0] || w__[16,23,0]) l261 l263
  skip
goto l253

l253: fi (w__[16,16,2]) l255 l257
  skip
goto l249

l249: fi (w__[16,16,1]) l251 l253
  skip
goto l243

l243: fi (w__[15,72,0]) l247 l249
  skip
goto l239

l239: fi (w__[15,15,2]) l241 l243
  skip
goto l235

l235: fi (w__[15,15,1]) l237 l239
  skip
goto l229

l229: fi (w__[14,15,0]) l233 l235
  skip
goto l225

l225: fi (w__[14,14,2]) l227 l229
  skip
goto l217

l217: fi (w__[14,14,1]) l223 l225
  skip
goto l211

l211: fi (w__[13,14,0] || w__[13,16,0]) l215 l217
  skip
goto l207

l207: fi (w__[13,13,2]) l209 l211
  skip
goto l203

l203: fi (w__[13,13,1]) l205 l207
  skip
goto l197

l197: fi (w__[12,13,0]) l201 l203
  skip
goto l189

l189: fi (w__[12,12,2]) l195 l197
  skip
goto l185

l185: fi (w__[12,12,1]) l187 l189
  skip
goto l179

l179: fi (w__[11,12,0]) l183 l185
  skip
goto l175

l175: fi (w__[11,11,2]) l177 l179
  skip
goto l171

l171: fi (w__[11,11,1]) l173 l175
  skip
goto l165

l165: fi (w__[10,11,0]) l169 l171
  skip
goto l161

l161: fi (w__[10,10,2]) l163 l165
  skip
goto l157

l157: fi (w__[10,10,1]) l159 l161
  skip
goto l151

l151: fi (w__[9,10,0]) l155 l157
  skip
goto l143

l143: fi (w__[9,9,2]) l149 l151
  skip
goto l139

l139: fi (w__[9,9,1]) l141 l143
  skip
goto l133

l133: fi (w__[8,9,0]) l137 l139
  skip
goto l129

l129: fi (w__[8,8,2]) l131 l133
  skip
goto l125

l125: fi (w__[8,8,1]) l127 l129
  skip
goto l119

l119: fi (w__[7,8,0]) l123 l125
  skip
goto l115

l115: fi (w__[7,7,2]) l117 l119
  skip
goto l111

l111: fi (w__[7,7,1]) l113 l115
  skip
goto l105

l105: fi (w__[6,9,0]) l109 l111
  skip
goto l101

l101: fi (w__[6,6,2]) l103 l105
  skip
goto l97

l97: fi (w__[6,6,1]) l99 l101
  skip
goto l91

l91: fi (w__[5,6,0]) l95 l97
  skip
goto l87

l87: fi (w__[5,5,2]) l89 l91
  skip
goto l79

l79: fi (w__[5,5,1]) l85 l87
  skip
goto l73

l73: fi (w__[4,5,0] || w__[4,7,0]) l77 l79
  skip
goto l69

l69: fi (w__[4,4,2]) l71 l73
  skip
goto l65

l65: fi (w__[4,4,1]) l67 l69
  skip
goto l59

l59: fi (w__[3,4,0]) l63 l65
  skip
goto l55

l55: fi (w__[3,3,2]) l57 l59
  skip
goto l51

l51: fi (w__[3,3,1]) l53 l55
  skip
goto l45

l45: fi (w__[2,3,0]) l49 l51
  skip
goto l41

l41: fi (w__[2,2,2]) l43 l45
  skip
goto l37

l37: fi (w__[2,2,1]) l39 l41
  skip
goto l31

l31: fi (w__[1,2,0]) l35 l37
  skip
goto l27

l27: fi (w__[1,1,2]) l29 l31
  skip
goto l21

l21: fi (w__[1,1,1]) l25 l27
  skip
goto l8

l22: from l8
  skip
goto l23

l23: from l22
  skip
goto l11

l8: from l21
  skip
if (w__[86,87,0]) l9 l22

l9: from l8
  w__[86,87,0] ^= 1
goto l6

l6: from l9
  skip
goto l7

l7: from l6
  skip
if (null w__) l1352 l1354

l1352: from l7
  skip
goto l1353

l1353: from l1352
  skip
goto l4

l1354: from l7
  skip
goto l1355

l1355: from l1354
  skip
goto l4

l4: fi (1) l1353 l1355
  skip
goto l5

l5: from l4
  free w__ [88,88,3]
goto l2

l2: from l5
  skip
goto l3

l3: from l2
  skip
exit
