
var _elm_lang$local_storage$Native_LocalStorage = function() {

function get(key)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var value = localStorage.getItem(key);
		callback(value === null
			? _elm_lang$core$Native_Scheduler.fail({ ctor: 'KeyNotFound', _0: key })
			: _elm_lang$core$Native_Scheduler.succeed(value)
		);
	});
}

function set(key, value)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		localStorage.setItem(key, value);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function remove(key)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		localStorage.removeItem(key);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

var clear = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	localStorage.clear();
	callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
});

var keys = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	var keyList = _elm_lang$core$Native_List.Nil;
	for (var i = localStorage.length; i--; )
	{
		keyList = _elm_lang$core$Native_List.Cons(localStorage.key(i), keyList);
	}
	callback(_elm_lang$core$Native_Scheduler.succeed(keyList));
});

return {
	get: get,
	set: F2(set),
	remove: remove,
	clear: clear,
	keys: keys
};

}();
