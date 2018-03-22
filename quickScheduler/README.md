# QuickScheduler and GreenNebula
Scheduling binaries on my PhD Thesis (2009-2013)

QS and GN were used as Schedulers for OpenNebula. Included load generators for WebService VMs, and Proxy/Gateway for routing client-VM queries also introducing network delays.

* Paper for QuickScheduler: http://dx.doi.org/10.1109/ICPP.2013.102
* paper for GreenNebula: http://dx.doi.org/10.1109/ICDCS.2014.53

## Files
These files are uploaded only to be contemplated by posterity. They are mostly hardcoded for the experiments and files (e.g. workload paths and experiment parameters). Do not attempt to run them at home, unless you discover the secrets that make them work.

### QuickSchedule
* **quickSchedule.java**: The scheduling application
* **Scheduler\*.java**: The different scheduling policies
* **ProxyThread.java**: Application acting as proxy (gateway) for applications queries
* **\*.data**: Parameter files
* **models\/\*.model**: Machine Learning models for ML Policy
* **genload\/genload.\***: Webservice Load Generators. Require a classic Apache HTTPD log to reproduce
* **fake\/\*.data**: Status information of cluster, for cluster simulation runs if no real cluster was available

### GreenNebula
* **greenNebula.java**: Extension of quickSchedule, implementing the GreenNebula Policies
* **greenNebula.conf**: Configuration for GreenNebula
* **datasets\/\*.data**: Meteo information for GreenNebula
