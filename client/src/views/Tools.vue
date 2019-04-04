<template>
	<div class="container">
		<whitelist></whitelist>
		<encrypt-privatekey></encrypt-privatekey>
	</div>
</template>

<script>
import Whitelist from '@/components/Tools/Whitelist.vue';
import EncryptPrivateKey from '@/components/Tools/EncryptPrivateKey.vue';

export default {
	name: 'Tools',
	components: {
		'whitelist': Whitelist,
		'encrypt-privatekey': EncryptPrivateKey
	},
	data() {
		return {};
	},
	created() {
		this.fetchData();
	},
	methods: {
		fetchData() {}
	}
};
</script>

<style scoped>
</style>
