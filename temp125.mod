
/* Sets */
	set hosts;
	set jobs;

/* parametres */
	param mcpu {h in hosts};
	param mmem {h in hosts};
	param jcpu {j in jobs};
	param jmem {j in jobs};
	param jcpumax {j in jobs}, integer;

	param rto {j in jobs};
	param req {j in jobs};
	param byt {j in jobs};
	param mbps {j in jobs};
	param tpq {j in jobs};

	param jold {h in hosts,j in jobs}, integer, binary;
	param news, integer;


/* Variables */
	var x {h in hosts, j in jobs}, integer, binary;
	var y1 {h in hosts}, binary;
	var y2 {h in hosts}, binary;
	var y3 {h in hosts}, binary;
	var y4 {h in hosts}, binary;

	var k {h in hosts, j in jobs}, >= 0, integer;
	var km {h in hosts, j in jobs}, >= 0, integer;
	var jcpuvar {j in jobs}, integer;
	var jmemvar {j in jobs}, integer;

	var hcpu {j in jobs} >= 0;
	var kcpu {h in hosts, j in jobs} >= 0;
	var cocup {h in hosts} >= 0;

	var hmem {j in jobs} >= 0;
	var kmem {h in hosts, j in jobs} >= 0;
	var mocup {h in hosts} >= 0;

	var v1 {j in jobs};
	var s1 {j in jobs} binary;
	var s2 {j in jobs} binary;
	var s3 {j in jobs} integer >= 0;
	var sy {j in jobs} >= 0;
	
	var sla {j in jobs} >= 0;
	var rt {j in jobs};

	var maquines; 				/* Variable dummy output */
	var power; 				/* Variable dummy output */
	var benefici; 				/* Benefici Resultant */
	var usedcpu {h in hosts};		/* CPU usada per host */

/* Objectiu */
	maximize benf : benefici;

/* Constraints */

	s.t. out0: benefici = ((sum{j in jobs} sla[j]) * 0.17) - (power * 9e-05);/* -	(((0.5 * sum{j in jobs, h in hosts} (if (jold[h,j]==1) then (1-x[h,j]) else x[h,j])) - 0.5*news) * 0.014);*/
	s.t. out1: maquines = sum{h in hosts} y1[h];
	s.t. out2: power = (sum{h in hosts} (y1[h]*267.8 + y2[h]*17.7 + y3[h]*17.0 + y4[h]*15.4));
	s.t. out3 {h in hosts}: usedcpu[h] = y1[h] + y2[h] + y3[h] + y4[h];

	/* Preu Job: 0.17 ; Preu Power: 0.00009: Penalty Migr: 0.014 */

	s.t. capacity {h in hosts} : cocup[h] <= (y1[h]+y2[h]+y3[h]+y4[h])*100;
	s.t. cpmemory {h in hosts} : mocup[h] <= mmem[h]*1000;

	s.t. maxcpu {h in hosts} : (y1[h]+y2[h]+y3[h]+y4[h]) <= mcpu[h];
	s.t. unicitat {j in jobs} : sum{h in hosts} x[h,j] = 1;
	s.t. power1 {h in hosts} : y1[h] >= y2[h];
	s.t. power2 {h in hosts} : y2[h] >= y3[h];
	s.t. power3 {h in hosts} : y3[h] >= y4[h];

	s.t. qos1 {h in hosts, j in jobs} : k[h,j] >= x[h,j];
	s.t. qos2 {h in hosts, j in jobs} : k[h,j] <= x[h,j] * 100000;
	s.t. qos3 {h in hosts, j in jobs} : k[h,j] - jcpuvar[j] <= (1 - x[h,j]);
	s.t. qos4 {h in hosts, j in jobs} : jcpuvar[j] - k[h,j] <= (1 - x[h,j]) * 100000;

	s.t. qos5 {j in jobs} : jcpuvar[j] <= jcpumax[j];
	s.t. qos6 {j in jobs} : jcpu[j] <= jcpuvar[j];

	s.t. qos7 {h in hosts, j in jobs} : km[h,j] >= x[h,j];
	s.t. qos8 {h in hosts, j in jobs} : km[h,j] <= x[h,j] * 100000;
	s.t. qos9 {h in hosts, j in jobs} : km[h,j] - jmemvar[j] <= (1 - x[h,j]);
	s.t. qos10 {h in hosts, j in jobs} : jmemvar[j] - km[h,j] <= (1 - x[h,j]) * 100000;

	s.t. qos11 {j in jobs} : jmemvar[j] <= jmem[j];
	s.t. qos12 {j in jobs} : jmem[j] * 1.0 <= jmemvar[j];

	s.t. sla0 {j in jobs} : v1[j] = (1 - ((rt[j] - rto[j])/ (1*rto[j])));

	s.t. sla1 {j in jobs} : rt[j] - rto[j] <= 100000 * (1 - s1[j]);
	s.t. sla2 {j in jobs} : rto[j] - rt[j] <= 100000 * s1[j];
	s.t. sla3 {j in jobs} : rt[j] - 2 * rto[j] <= 100000 * (1 - s2[j]);
	s.t. sla4 {j in jobs} : 2 * rto[j] - rt[j] <= 100000 * s2[j];

	s.t. sla5 {j in jobs} : s3[j] = s2[j] - s1[j];

	s.t. sla6 {j in jobs} : sy[j] >= -1 * s3[j];
	s.t. sla7 {j in jobs} : sy[j] <= 100000 * s3[j];
	s.t. sla8 {j in jobs} : sy[j] - v1[j] <= 100000 * (1 - s3[j]);
	s.t. sla9 {j in jobs} : v1[j] - sy[j] <= 100000 * (1 - s3[j]);

	s.t. sla10 {j in jobs} : sla[j] = sy[j] + s1[j];

	s.t. rt01 {j in jobs} : rt[j] =  3.950e-03
					+ ( 1.087e-05 * req[j])
					+ (-4.147e-08 * byt[j])
					+ ( 2.954e-05 * hcpu[j])
					+ (-1.919e-04 * mbps[j])
					+ ( 1.906e-08 * tpq[j])
					+ (-3.633e-05 * jcpuvar[j])
					+ ( 4.762e-06 * jcpu[j]);
	
	s.t. cpu1 {h in hosts} :		cocup[h] = sum{i in jobs}  k[h,i];
	s.t. cpu2 {h in hosts, j in jobs} : 	kcpu[h,j] >= x[h,j];
	s.t. cpu3 {h in hosts, j in jobs} :	kcpu[h,j] <= x[h,j] * 100000;
	s.t. cpu4 {h in hosts, j in jobs} :	kcpu[h,j] - cocup[h] <= (1 - x[h,j]);
	s.t. cpu5 {h in hosts, j in jobs} :	cocup[h] - kcpu[h,j] <= (1 - x[h,j]) * 100000;
	s.t. cpu6 {j in jobs} :			hcpu[j] = sum{h in hosts}  kcpu[h,j];

	s.t. mem1 {h in hosts} :		mocup[h] = sum{i in jobs}  km[h,i];
	s.t. mem2 {h in hosts, j in jobs} : 	kmem[h,j] >= x[h,j];
	s.t. mem3 {h in hosts, j in jobs} :	kmem[h,j] <= x[h,j] * 100000;
	s.t. mem4 {h in hosts, j in jobs} :	kmem[h,j] - mocup[h] <= (1 - x[h,j]);
	s.t. mem5 {h in hosts, j in jobs} :	mocup[h] - kmem[h,j] <= (1 - x[h,j]) * 100000;
	s.t. mem6 {j in jobs} :			hmem[j] = sum{h in hosts}  kmem[h,j];
 
end;
