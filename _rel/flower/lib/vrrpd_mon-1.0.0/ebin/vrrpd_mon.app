{application, vrrpd_mon,
  [{description,"Interface to VRRP daemon - reacts to VRRP state changes"},
   {vsn,"1.0.0"},
   {registered,[]},
   {applications,[kernel,stdlib]},
   {mod,{vrrpd_mon,[]}},
   {env,[]},
   {modules,[vrrpd_mon_srv]}]
}.