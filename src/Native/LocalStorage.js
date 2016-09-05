
var _elm_lang$persistent_cache$Native_LocalStorage = function() {

if (!localStorage || !localStorage.getItem || !localStorage.setItem)
{
	function disabled()
	{
		return _elm_lang$core$Native_Scheduler.fail({ ctor: 'Disabled' });
	}

	return {
		get: disabled,
		set: F2(disabled),
		remove: disabled,
		clear: disabled(),
		keys: disabled()
	};
}

function get(key)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var value = localStorage.getItem(key);
		callback(_elm_lang$core$Native_Scheduler.succeed(
			value === null
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(value)
		));
	});
}

function set(key, value)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		try
		{
			localStorage.setItem(key, value);
			return callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
		}
		catch (e)
		{
			return callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'QuotaExceeded' }));
		}
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
