 
 The contents of the subdirectories are as follows:
 
 grid: This directory contains sample grid files for the U of Canterbury flume. All files are in a format used by a grid generation program TQGG (available in the public repository https://github.com/rrusk/TQGG.git).
 
      files in this directory:
      flume.nod - is a file containing the outline of the flume and can be used to create grids.
      grid_points.ngh - is a ngh format file listing the locations of the grid points with 1 cm resolution. After the header information, each line contains
      
      node number, x, y, code, z , node adjacency
      
      element_list.ele - is a file containing quadrilateral element connections for FE models. Each line contains the 4 nodes in CCW order and an element code.
      
      
 location: This directory contains location vs time data for the experimental test cases.
