#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <cmath>

using namespace Rcpp;

const double EARTH_RADIUS_M = 6371000.0;

inline double deg2rad(double deg) {
  return deg * M_PI / 180.0;
}

inline double haversine_m(double lon1, double lat1, double lon2, double lat2) {
  double phi1 = deg2rad(lat1);
  double phi2 = deg2rad(lat2);
  double dphi = deg2rad(lat2 - lat1);
  double dlambda = deg2rad(lon2 - lon1);
  
  double a = std::sin(dphi / 2.0) * std::sin(dphi / 2.0) +
    std::cos(phi1) * std::cos(phi2) *
    std::sin(dlambda / 2.0) * std::sin(dlambda / 2.0);
  
  double c = 2.0 * std::atan2(std::sqrt(a), std::sqrt(1.0 - a));
  return EARTH_RADIUS_M * c;
}

// [[Rcpp::export]]
DataFrame nearest_haversine_k_cpp(
    DataFrame xyz,
    DataFrame stations,
    int k = 1
) {
  CharacterVector input_id = xyz["id"];
  NumericVector input_lon = xyz["lon"];
  NumericVector input_lat = xyz["lat"];
  
  CharacterVector station_id = stations["id"];
  NumericVector station_lon = stations["lon"];
  NumericVector station_lat = stations["lat"];
  
  int n_input = input_id.size();
  int n_station = station_id.size();
  
  if (input_lon.size() != n_input || input_lat.size() != n_input) {
    stop("input_id, input_lon, and input_lat must have the same length");
  }
  if (station_lon.size() != n_station || station_lat.size() != n_station) {
    stop("station_id, station_lon, and station_lat must have the same length");
  }
  if (k < 1) {
    stop("k must be >= 1");
  }
  if (k > n_station) {
    k = n_station;
  }
  
  std::vector<std::string> out_input_id;
  std::vector<double> out_input_lon;
  std::vector<double> out_input_lat;
  std::vector<std::string> out_station_id;
  std::vector<double> out_station_lon;
  std::vector<double> out_station_lat;
  std::vector<double> out_distance_m;
  std::vector<int> out_rank;
  
  out_input_id.reserve((size_t)n_input * k);
  out_input_lon.reserve((size_t)n_input * k);
  out_input_lat.reserve((size_t)n_input * k);
  out_station_id.reserve((size_t)n_input * k);
  out_station_lon.reserve((size_t)n_input * k);
  out_station_lat.reserve((size_t)n_input * k);
  out_distance_m.reserve((size_t)n_input * k);
  out_rank.reserve((size_t)n_input * k);
  
  for (int i = 0; i < n_input; i++) {
    // if (NumericVector::is_na(input_lon[i]) || NumericVector::is_na(input_lat[i])) {
    //   for (int r = 1; r <= k; r++) {
    //     out_input_id.push_back(Rcpp::as<std::string>(input_id[i]));
    //     out_input_lon.push_back(input_lon[i]);
    //     out_input_lat.push_back(input_lat[i]);
    //     //out_station_id.push_back(NA_STRING);
    //     out_station_lon.push_back(NA_REAL);
    //     out_station_lat.push_back(NA_REAL);
    //     out_distance_m.push_back(NA_REAL);
    //     out_rank.push_back(r);
    //   }
    //   continue;
    // }
    
    std::vector< std::pair<double, int> > dists;
    dists.reserve(n_station);
    
    for (int j = 0; j < n_station; j++) {
      if (NumericVector::is_na(station_lon[j]) || NumericVector::is_na(station_lat[j])) {
        continue;
      }
      
      double d = haversine_m(input_lon[i], input_lat[i], station_lon[j], station_lat[j]);
      dists.push_back(std::make_pair(d, j));
    }
    
    int kk = std::min(k, (int)dists.size());
    
    std::partial_sort(
      dists.begin(),
      dists.begin() + kk,
      dists.end(),
      [](const std::pair<double, int>& a, const std::pair<double, int>& b) {
        return a.first < b.first;
      }
    );
    
    for (int r = 0; r < kk; r++) {
      int j = dists[r].second;
      
      out_input_id.push_back(Rcpp::as<std::string>(input_id[i]));
      out_input_lon.push_back(input_lon[i]);
      out_input_lat.push_back(input_lat[i]);
      out_station_id.push_back(Rcpp::as<std::string>(station_id[j]));
      out_station_lon.push_back(station_lon[j]);
      out_station_lat.push_back(station_lat[j]);
      out_distance_m.push_back(dists[r].first);
      out_rank.push_back(r + 1);
    }
    
    // pad with NA if all/most stations were missing
    for (int r = kk; r < k; r++) {
      out_input_id.push_back(Rcpp::as<std::string>(input_id[i]));
      out_input_lon.push_back(input_lon[i]);
      out_input_lat.push_back(input_lat[i]);
      out_station_id.push_back("Missing");
      out_station_lon.push_back(NA_REAL);
      out_station_lat.push_back(NA_REAL);
      out_distance_m.push_back(NA_REAL);
      out_rank.push_back(r + 1);
    }
  }
  
  return DataFrame::create(
    _["input_id"] = out_input_id,
    _["station_id"] = out_station_id,
    _["distance"] = out_distance_m,
    _["rank"] = out_rank,
    _["stringsAsFactors"] = false
  );
}