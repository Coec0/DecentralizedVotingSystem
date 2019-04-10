<template>
	<div class="container">
		<div class="tools col-md-8">
			<whitelist-add></whitelist-add>
			<whitelist-check></whitelist-check>
			<whitelist-enable></whitelist-enable>
			<whitelist-disable></whitelist-disable>
			<encrypt-privatekey></encrypt-privatekey>
			<results-add></results-add>
		</div>
	</div>
</template>

<script>
import WhitelistAdd from '@/components/Tools/WhitelistAdd.vue';
import WhitelistCheck from '@/components/Tools/WhitelistCheck.vue';
import WhitelistEnable from '@/components/Tools/WhitelistEnable.vue';
import WhitelistDisable from '@/components/Tools/WhitelistDisable.vue';
import EncryptPrivateKey from '@/components/Tools/EncryptPrivateKey.vue';
import ResultsAdd from '@/components/Tools/ResultsAdd.vue';

export default {
	name: 'Tools',
	components: {
		'whitelist-add': WhitelistAdd,
		'whitelist-check': WhitelistCheck,
		'whitelist-disable': WhitelistDisable,
		'whitelist-enable': WhitelistEnable,
		'encrypt-privatekey': EncryptPrivateKey,
		'results-add': ResultsAdd
	}
};
</script>

<style scoped>
h1 {
	font-size: 5rem;
	margin-top: 1em;
	margin-bottom: 1em;
}

.tools {
	padding-top: 50px;
	display: flex;
	flex-grow: 1;
	flex-wrap: wrap;
	justify-content: center;
	align-content: flex-start;
	margin-left: auto;
	margin-right: auto;
}

.tools div {
	  transition: box-shadow .3s;
	  margin-top: 25px;
	  border-radius:10px;
	  border: 1px solid #ccc;
	  background: #fff;
}

.tools div:hover {
  box-shadow: 0 0 11px rgba(33,33,33,.2); 
}
</style>
