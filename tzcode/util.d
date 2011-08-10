module tzcode.util;

class Util {
static:
public:

	uint isFirst(uint dayOfWeek, uint afterOrOnDay, long year, uint month) {
		return false;
	}

	uint isLast(uint dayOfWeek, long year, uint month) {
		return false;
	}

	bool isAfter (long year, uint month, uint day, 
			uint hour, uint minute,  
			long check_year, uint check_month, uint check_day, 
			uint check_hour, uint check_minute) {

		if (year >= check_year) {
			if (month > check_month) {
				return true;
			}
			else if (month == check_month) {
				if (day > check_day) {
					return true;
				}
				else if (day == check_day) {
					if (hour > check_hour) {
						return true;
					}
					else if (hour == check_hour) {
						if (minute >= check_minute) {
							return true;
						}
					}
				}
			}
		}

		return false;
	}

	bool isBefore (long year, uint month, uint day, 
			uint hour, uint minute, 
			long check_year, uint check_month, uint check_day, 
			uint check_hour, uint check_minute) {

		if (year <= check_year) {
			if (month < check_month) {
				return true;
			}
			else if (month == check_month) {
				if (day < check_day) {
					return true;
				}
				else if (day == check_day) {
					if (hour < check_hour) {
						return true;
					}
					else if (hour == check_hour) {
						if (minute < check_minute) {
							return true;
						}
					}
				}
			}
		}

		return false;
	}
}
